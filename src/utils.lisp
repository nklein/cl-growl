(in-package :growl)

(defmacro with-utf-8-strings ((&rest names) &body body)
  (labels ((convert-name (nn)
	     (cond
	       ((listp nn) `(,(first nn)
			      (when ,(second nn)
				(trivial-utf-8:string-to-utf-8-bytes
				    ,(second nn)))))
	       (t `(,nn (when ,nn
			  (trivial-utf-8:string-to-utf-8-bytes ,nn)))))))
    `(let ,(mapcar #'convert-name names)
       ,@body)))

(defmacro with-output-to-binary-string (&body body)
  (let ((stream (gensym "STREAM-")))
    `(let ((,stream (flexi-streams:make-in-memory-output-stream)))
       (let ((*standard-output* ,stream))
	 ,@body)
       (flexi-streams:get-output-stream-sequence ,stream))))

(defun write-short (ss stream)
  (write-byte (ldb (byte 8 8) ss) stream)
  (write-byte (ldb (byte 8 0) ss) stream))

#+ironclad
(defun sha1 (buffer password)
  (let ((digester (ironclad:make-digest :sha1)))
    (ironclad:update-digest digester buffer)
    (ironclad:update-digest digester password)
    (ironclad:produce-digest digester)))

#+ironclad
(defun sha256 (buffer password)
  (let ((digester (ironclad:make-digest :sha256)))
    (ironclad:update-digest digester buffer)
    (ironclad:update-digest digester password)
    (ironclad:produce-digest digester)))

#+(or md5 ironclad)
(defun md5 (buffer password)
  #+ironclad
  (let ((digester (ironclad:make-digest :md5)))
    (ironclad:update-digest digester buffer)
    (when password
      (ironclad:update-digest digester password))
    (ironclad:produce-digest digester))
  #-ironclad
  (let ((digester (md5:make-md5-state)))
    (md5:update-md5-state digester buffer)
    (when password
      (md5:update-md5-state digester password))
    (md5:finalize-md5-state digester)))

(defun noauth (buffer password)
  (declare (ignore buffer password))
  (make-array '(0) :element-type '(unsigned-byte 8)
	           :initial-element 0))

(defun checksum (payload password checksum-mode)
  (let ((payload (if (typep payload '(simple-array (unsigned-byte 8) (*)))
		     payload
		     (string-to-utf-8-bytes payload)))
	(password (if (typep password '(simple-array (unsigned-byte 8) (*)))
		      password
		      (string-to-utf-8-bytes password))))
    (case checksum-mode
      #+ironclad (:sha256 (sha256 payload password))
      #+ironclad (:sha1 (sha1 payload password))
      #+(or md5 ironclad) (:md5 (md5 payload password))
      (:none (noauth payload password))
      (otherwise (error 'unavailable-checksum-error
			:requested-mode checksum-mode)))))

(defun pkcs7-pad (payload block-len)
  (let* ((payload-len (length payload))
	 (left        (mod payload-len block-len))
	 (pad-len     (if (zerop left) 8 left))
	 (pad         (loop :for ii :from 1 :to pad-len
			    :collecting pad-len)))
    (concatenate '(array (unsigned-byte 8) (*))
		 payload
		 pad)))

#+ironclad
(defun ironclad-encrypt (name payload key iv)
  (let* ((cipher (ironclad:make-cipher name
				       :key key
				       :mode :cbc
				       :initialization-vector iv))
	 (blen (ironclad:block-length cipher))
	 (data (pkcs7-pad payload blen))
	 (out (make-array (list (length data))
			  :element-type '(unsigned-byte 8))))
    (push (list (length payload) (length out)
		(utf-8-bytes-to-string payload)
		(map 'list #'identity key)) *written*)
    (ironclad:encrypt cipher data out)
    out))

#+ironclad
(defun aes (payload key iv)
  (ironclad-encrypt :aes payload key iv))

#+ironclad
(defun des (payload key iv)
  (ironclad-encrypt :des payload key iv))

#+ironclad
(defun 3des (payload key iv)
  (ironclad-encrypt :3des payload key iv))


(defun encrypt (payload encryption-mode key iv)
  (let ((payload (if (typep payload '(simple-array (unsigned-byte 8) (*)))
		     payload
		     (make-array (list (length payload))
				 :element-type '(unsigned-byte 8)
				 :initial-contents payload))))
    (case encryption-mode
      #+ironclad (:aes  (aes  payload (subseq key 0 24) iv))
      #+ironclad (:des  (des  payload (subseq key 0 8) iv))
      #+ironclad (:3des (3des payload (subseq key 0 24) iv))
      (:none payload)
      (otherwise (error 'unavailable-encryption-error
			:requested-mode encryption-mode)))))

(defun hex-encode (bytes)
  (flet ((nibble-to-hex (nn)
	   (elt "0123456789ABCDEF" nn)))
    (with-output-to-string (ss)
      (map nil #'(lambda (bb)
		   (format ss "~C~C" (nibble-to-hex (ldb (byte 4 4) bb))
			             (nibble-to-hex (ldb (byte 4 0) bb))))
	   bytes))))

(defun generate-random-bytes (byte-count)
  (let ((array (make-array (list byte-count)
			   :element-type '(unsigned-byte 8))))
    (loop :for ii :from 0 :below byte-count
       :do (setf (aref array ii) (random 256)))
    array))

(defun generate-salt (salt-byte-count)
  (generate-random-bytes salt-byte-count))

(defun generate-iv (encryption-mode)
  (case encryption-mode
    (:none (generate-random-bytes 0))
    #+ironclad (:aes  (generate-random-bytes 16))
    #+ironclad (:des  (generate-random-bytes 8))
    #+ironclad (:3des (generate-random-bytes 8))))

(defun generate-unique-id (item)
  #+(or md5 ironclad) (nth-value 0 (intern (hex-encode (md5 item nil))))
  #-(or md5 ironclad) (symbol-name (gensym "UNIQUE-ID-")))
