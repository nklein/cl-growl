(in-package :growl)

(defun register (&key (app *growl-default-app*)
		      (app-icon *growl-default-app-icon*)
		      enabled
		      disabled
		      (origin-fields *growl-default-origin-fields*)
		      (custom-fields *growl-default-custom-fields*)
		      (application-fields *growl-default-application-fields*)
		      (host *growl-default-host*)
		      (port *growl-default-port*)
		      (checksum-mode *growl-default-checksum-mode*)
		      (encryption-mode *growl-default-encryption-mode*)
		      (password *growl-default-password*))

  "Register as the application named APP with the ENABLED
   notifications turned on and the DISABLED notifications turned off
   by default.

   The ENABLED and DISABLED parameters are lists.  The list elements
   of the ENABLED and DISABLED parameters can be either a string or a
   list.  If it is a string, it is the name of a notification.  If it
   is a list, it can have either one, two, or three members.  The
   first member is the notification name, the second member (if
   present) is the human-readable notification name, and the third
   member (if present) is the notification icon.  The third item can
   be either a URL to the notification icon or a one-dimensional
   binary vector containing the encoded icon image."


  ;; sanity checks
  (required-string app)
  (optional-icon app-icon)
  (mapc #'valid-notice-decl enabled)
  (mapc #'valid-notice-decl disabled)
  (mapc #'valid-origin-field origin-fields)
  (mapc #'valid-field custom-fields)
  (mapc #'valid-field application-fields)
  (required-string host)
  (valid-port port)

  (valid-encryption-checksum-combo encryption-mode checksum-mode)

  (unless (and (eql checksum-mode :none)
	       (eql encryption-mode :none))
    (required-string password))

  (labels ((hdr (data-hash)
	     (hdr-line "Application-Name" app data-hash)
	     (when app-icon
	       (hdr-line "Application-Icon" app-icon data-hash))
	     (mapc #'(lambda (ff) (origin-hdr-line ff data-hash))
		   origin-fields)
	     (mapc #'(lambda (ff) (custom-hdr-line ff data-hash))
		   custom-fields)
	     (mapc #'(lambda (ff) (app-hdr-line ff data-hash))
		   application-fields)
	     (hdr-line "Notifications-Count" (+ (length enabled)
						(length disabled))
		       data-hash)
	     (hdr-terpri)
	     (mapc #'(lambda (nn) (notice-decl nn t data-hash)) enabled)
	     (mapc #'(lambda (nn) (notice-decl nn nil data-hash)) disabled)))
    (let* ((data-hash (make-hash-table))
	   (msg (with-output-to-binary-string
		  (compose +growl-register-message-type+
			   :header (with-output-to-binary-string
				     (hdr data-hash))
			   :binary-data data-hash
			   :checksum-mode checksum-mode
			   :encryption-mode encryption-mode
			   :password password))))
      (let ((sock (usocket:socket-connect host port
					  :element-type '(unsigned-byte 8))))
	(unwind-protect
	     (progn
	       (write-sequence msg (usocket:socket-stream sock))
	       (force-output (usocket:socket-stream sock))
	       (echo-all-bytes (usocket:socket-stream sock)))
	  (usocket:socket-close sock)))))
  (values))
