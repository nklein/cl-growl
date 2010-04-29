(in-package :growl)

(define-condition unavailable-checksum-error (error)
  ((requested-mode :initarg :requested-mode
		   :reader unavailable-checksum-requested-mode))
  (:report (lambda (condition stream)
	     (case (unavailable-checksum-requested-mode condition)
	       (:sha256 (format stream "Need IRONCLAD package to do SHA256 checksum"))
	       (:md5    (format stream "Need IRONCLAD or MD5 package to do MD5 checksum"))
	       (otherwise (format stream "Unknown checksum mode: ~S"
				  (unavailable-checksum-requested-mode condition)))))))

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
    (ironclad:update-digest digester password)
    (ironclad:produce-digest digester))
  #-ironclad
  (let ((digester (md5:make-md5-state)))
    (md5:update-md5-state digester buffer)
    (md5:update-md5-state digester password)
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
    (usocket:with-udp-client-socket (ss nil nil
					:element-type '(unsigned-byte 8))
      (usocket:socket-send ss buffer (length buffer) :host host :port port))))
