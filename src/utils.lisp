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
		     (make-array (length payload)
				 :element-type '(unsigned-byte 8)
				 :initial-contents payload)))
	(password (if (typep password '(simple-array (unsigned-byte 8) (*)))
		      password
		      (make-array (length password)
				  :element-type '(unsigned-byte 8)
				  :initial-contents password))))
    (case checksum-mode
      #+ironclad (:sha256 (sha256 payload password))
      #+(or md5 ironclad) (:md5 (md5 payload password))
      (:noauth (noauth payload password))
      (otherwise (error 'unavailable-checksum-error
			:requested-mode checksum-mode)))))

(defun hex-encode (bytes)
  (flet ((nibble-to-hex (nn)
	   (elt "0123456789ABCDEF" nn)))
    (with-output-to-string (ss)
      (map nil #'(lambda (bb)
		   (format ss "~C~C" (nibble-to-hex (ldb (byte 4 4) bb))
			             (nibble-to-hex (ldb (byte 4 0) bb))))
	   bytes))))

(defun generate-salt (salt-byte-count)
  (let ((s "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz./"))
    (trivial-utf-8:string-to-utf-8-bytes
       (map 'string #'identity (loop :for ii :from 1 :to salt-byte-count
				  :with len = (length s)
				  :collecting (elt s (random len)))))))

(defun generate-unique-id (item)
  #+(or md5 ironclad) (nth-value 0 (intern (hex-encode (md5 item nil))))
  #-(or md5 ironclad) (symbol-name (gensym "UNIQUE-ID-")))

(defun send-packet (payload &key host port checksum-mode password
		                 #+(and ironclad notyet) encryptp
		      &aux (password-enc (string-to-utf-8-bytes password)))
  (let ((buffer (concatenate '(simple-array (unsigned-byte 8) (*))
			     payload
			     (checksum payload password-enc checksum-mode))))
    #+(and ironclad notyet)
    (when encryptp
      (let ((cipher (ironclad:make-cipher :aes
					  :key password-enc
					  :mode :cbc)))
	(ironclad:encrypt-in-place cipher buffer :start 1)))
    (usocket:with-udp-client-socket (ss host port
					:element-type '(unsigned-byte 8))
      (usocket:socket-send ss buffer (length buffer)))))
