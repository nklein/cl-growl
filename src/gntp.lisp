(in-package :growl)

;;; =======================================================
;;; validity checks
;;; =======================================================
(defun valid-encryption-checksum-combo (encryption-mode checksum-mode)
  (unless (typep checksum-mode 'growl-checksum-mode)
    (error 'unavailable-checksum-mode-error :requested-mode checksum-mode))

  (unless (typep encryption-mode 'growl-encryption-mode)
    (error 'unavailable-encryption-mode-error :requested-mode encryption-mode))

  (case encryption-mode
    #+ironclad (:des (when (member checksum-mode '(:none))
		       (error 'incompatible-encryption-and-checksum-error
			      :requested-encryption-mode encryption-mode
			      :requested-checksum-mode checksum-mode)))
    #+ironclad (:aes (when (member checksum-mode '(:none :md5 :sha1))
		       (error 'incompatible-encryption-and-checksum-error
			      :requested-encryption-mode encryption-mode
			      :requested-checksum-mode checksum-mode)))
    #+ironclad (:3des (when (member checksum-mode '(:none :md5 :sha1))
			(error 'incompatible-encryption-and-checksum-error
			       :requested-encryption-mode encryption-mode
			       :requested-checksum-mode checksum-mode)))))

(defun required-string (ss)
  (assert (stringp ss))
  (assert (plusp (length ss)))
  (assert (not (find #\Return ss))))

(defun optional-string (ss)
  (unless (null ss)
    (required-string ss)))

(defun optional-icon (ii)
  (cond
    ((typep ii 'growl-binary-data-type)
     (assert (plusp (length ii))))
    (t (optional-string ii))))

(defun required-id (id)
  (cond
    ((symbolp id) t)
    (t (required-string id))))

(defun optional-id (id)
  (cond
    ((null id) t)
    (t (required-id id))))

(defun valid-priority (pp)
  (assert (<= -2 pp 2)))

(defun required-value (vv)
  (cond
    ((typep vv 'growl-binary-data-type) (assert (plusp (length vv))))
    ((stringp vv) (required-string vv))
    (t (assert (not (null vv))))))

(defun valid-field (ff)
  (required-string (first ff))
  (required-value (second ff)))

(defun valid-origin-field (ff)
  (valid-field ff)
  (assert (member (first ff) '("Machine-Name"
			       "Software-Name"
			       "Software-Version"
			       "Platform-Name"
			       "Platform-Version")
		  :test #'string=)))

(defun valid-notice-decl (nn)
  (cond
    ((listp nn) (destructuring-bind (name &optional readable-name icon
					  &rest options) nn
		  (required-string name)
		  (optional-string readable-name)
		  (optional-icon icon)
		  (mapc #'valid-field options)))
    (t (required-string nn))))

(defun valid-port (pp)
  (assert (plusp pp)))

(defun require-salt (salt)
  (typecase salt
    (function (let ((val (funcall salt)))
                (if (functionp val)
                    (error 'unsupported-salt-type-error
                           :salt salt)
                    (require-salt val))))
    (string (string-to-utf-8-bytes salt))
    (growl-binary-data-type salt)
    (t (error 'unsupported-salt-type-error :salt))))

(defun require-iv (iv encryption-mode)
  (typecase iv
    (function (let ((val (funcall iv encryption-mode)))
                (if (functionp val)
                    (error 'unsupported-iv-type-error
                           :salt iv)
                    (require-iv val encryption-mode))))
    (string (string-to-utf-8-bytes iv))
    (growl-binary-data-type iv)
    (t (error 'unsupported-iv-type-error
              :iv iv :encryption-mode encryption-mode))))
  
;;; =======================================================
;;; protocol element output
;;; =======================================================
(defun hdr-terpri ()
  (write-byte (char-code #\Return) *standard-output*)
  (write-byte (char-code #\Newline) *standard-output*))

(defun hdr-line (key value data-hash)
  (with-utf-8-strings (key (colon ": "))
    (write-sequence key *standard-output*)
    (write-sequence colon *standard-output*))
  (cond
    ((typep value 'growl-binary-data-type)
     (let ((uid (generate-unique-id value)))
       (setf (gethash uid data-hash) value)
       (with-utf-8-strings ((uid (concatenate 'string
					      "x-growl-resource://"
					      (symbol-name uid))))
	 (write-sequence uid *standard-output*))))
    ((stringp value) (with-utf-8-strings (value)
		       (write-sequence value *standard-output*)))
    (t (with-utf-8-strings ((vv (format nil "~A" value)))
	 (write-sequence vv *standard-output*))))
  (hdr-terpri))

(defun origin-hdr-line (hdr data-hash)
  (with-utf-8-strings ((origin "Origin-"))
    (write-sequence origin *standard-output*))
  (hdr-line (first hdr) (second hdr) data-hash))

(defun custom-hdr-line (hdr data-hash)
  (with-utf-8-strings ((x "X-"))
    (write-sequence x *standard-output*))
  (hdr-line (first hdr) (second hdr) data-hash))

(defun app-hdr-line (hdr data-hash)
  (with-utf-8-strings ((data "Data-"))
    (write-sequence data *standard-output*))
  (hdr-line (first hdr) (second hdr) data-hash))

(defun notice-decl (nn enabled data-hash)
  (flet ((notice-enabled ()
	   (hdr-line "Notification-Enabled"
		     (if enabled "Yes" "No") data-hash)))
    (cond
      ((listp nn) (destructuring-bind (name &optional readable-name icon
					    &rest others) nn
		    (hdr-line "Notification-Name" name data-hash)
		    (when readable-name
		      (hdr-line "Notification-Display-Name" readable-name
				data-hash))
		    (when icon
		      (hdr-line "Notification-Icon" icon data-hash))
		    (notice-enabled)
		    (mapc #'(lambda (oo) (custom-hdr-line oo data-hash))
			  others)))
      (t (hdr-line "Notification-Name" nn data-hash)
	 (notice-enabled))))
  (hdr-terpri))

(defun encode-binary-data (id value &key encryption-mode key iv)
  (with-output-to-binary-string
    (hdr-line "Identifier" id nil)
    (let ((enc (encrypt value encryption-mode key iv)))
      (hdr-line "Length" (length enc) nil)
      (hdr-terpri)
      (write-sequence enc *standard-output*))
    (hdr-terpri)))

(defun make-key (checksum-mode password salt)
  (with-utf-8-strings (password)
    (checksum password salt checksum-mode)))

(defun make-encryption-hdr (encryption-mode iv)
  (case encryption-mode
    (:none     "NONE")
    (otherwise (format nil "~A:~A" encryption-mode (hex-encode iv)))))

(defun make-password-hash-hdr (key checksum-mode salt)
  (case checksum-mode
    (:none "NONE")
    (otherwise (format nil "~A:~A.~A" checksum-mode
		                      (hex-encode (checksum key ""
							    checksum-mode))
				      (hex-encode salt)))))

(defun compose (message-type &key header binary-data
		                  checksum-mode encryption-mode password
		                  salt iv)
  (let* ((key (make-key checksum-mode password salt))
	 (enc-hdr (make-encryption-hdr encryption-mode iv))
	 (pwd-hdr (make-password-hash-hdr key checksum-mode salt))
	 (packet-start (format nil "GNTP/~A ~A ~A ~A"
			           +growl-protocol-version+
				   message-type
				   enc-hdr
				   pwd-hdr)))
    (with-utf-8-strings (packet-start)
      (write-sequence packet-start *standard-output*))
    (hdr-terpri)

    (write-sequence (encrypt header encryption-mode key iv) *standard-output*)

    (when binary-data
      (unless (or (eql encryption-mode :none)
		  (zerop (hash-table-count binary-data)))
	(hdr-terpri)
	(hdr-terpri))
      (maphash #'(lambda (id value)
		   (let ((dd (encode-binary-data id value
				      :encryption-mode encryption-mode
				      :key key
				      :iv iv)))
		     (write-sequence dd *standard-output*)))
	       binary-data))
    (hdr-terpri)
    (hdr-terpri))
  salt)