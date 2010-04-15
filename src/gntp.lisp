(in-package :growl)

;;; =======================================================
;;; validity checks
;;; =======================================================
(defun required-string (ss)
  (assert (typep ss 'string))
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
       (with-utf-8-strings ((uid (symbol-name uid)))
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
  (hdr-terpri)
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
	 (notice-enabled)))))

(defun encode-binary-data (id value &key encryption-mode password salt iv)
  (declare (ignore encryption-mode password salt iv))
  (with-output-to-binary-string
    (hdr-terpri)
    (hdr-line "Identifier" id nil)
    (hdr-line "Length" (length value) nil)
    (hdr-terpri)
    (write-sequence value *standard-output*)
    (hdr-terpri)))
