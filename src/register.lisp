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

  (unless (eql encryption-mode :none)
    (assert (not (eql checksum-mode :none))))

  (unless (and (eql checksum-mode :none)
	       (eql encryption-mode :none))
    (required-string password))

  (unless (typep checksum-mode 'growl-checksum-mode)
    (error 'unavailable-checksum-mode-error :requested-mode checksum-mode))

  (unless (typep encryption-mode 'growl-encryption-mode)
    (error 'unavailable-encryption-mode-error :requested-mode encryption-mode))


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
	     (mapc #'(lambda (nn) (notice-decl nn t data-hash)) enabled)
	     (mapc #'(lambda (nn) (notice-decl nn nil data-hash)) disabled)))
    (let* ((data-hash (make-hash-table))
	   (header (with-output-to-binary-string (hdr data-hash)))
	   (binary-data nil))
      (format t "~A" (trivial-utf-8:utf-8-bytes-to-string header))
      (maphash #'(lambda (id value)
		   (push (encode-binary-data id value
					     :encryption-mode encryption-mode
					     :password password)
			 binary-data))
	       data-hash)
      (mapc #'(lambda (bb)
		(format t "~A" (trivial-utf-8:utf-8-bytes-to-string bb)))
	    binary-data)))
  (values))
