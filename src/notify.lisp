(in-package :growl)

(defun notify (body &key (app *growl-default-app*)
			 (notification *growl-default-notification*)
			 (id (gensym "NOTICE-"))
			 (title *growl-default-title*)
			 (sticky nil)
			 (priority *growl-default-priority*)
			 (icon *growl-default-icon*)
			 (coalesce nil)
			 (callback-context *growl-default-callback-context*)
			 (callback-context-type
			        *growl-default-callback-context-type*)
			 (callback-target *growl-default-callback-target*)
			 (host *growl-default-host*)
			 (port *growl-default-port*)
			 (checksum-mode *growl-default-checksum-mode*)
			 (encryption-mode *growl-default-encryption-mode*)
			 (password *growl-default-password*))

  "Send a notification with TITLE as its title and BODY as its text
   body.  The notificiation is from the APP and with notification name
   NOTIFICATION.  It has an ID that is hopefully unique to this
   notification.  And, it has an advisory PRIORITY on the range
   [-2,2] (unless PRIORITY is nil).  The app can request that the
   notification be STICKY.

   This notice can specify its own ICON either by URL or by sending
   along the binary data.

   If a COALESCE is given, it should contain the ID of a previously
   sent notification that should be updated or replaced by this one.

   If a CALLBACK-CONTEXT string is given, it specifies that GROWL
   should send a message back on this notification's socket when the
   user click the message, closes the message, or when the message
   times out.  If CALLBACK-CONTEXT is non-nil (and non-empty), then
   CALLBACK-CONTEXT-TYPE must also be non-nil (and non-empty).  The
   CALLBACK-CONTEXT and CALLBACK-CONTEXT-TYPE are returned in the
   callback.

   If a CALLBACK-TARGET is specified, it must be a URL.  If the user
   clicks on a notification with a CALLBACK-TARGET, the user's default
   browser is opened to the CALLBACK-TARGET URL.

   This function returns (VALUES STREAM ID).  STREAM will be NIL if
   there is no CALLBACK-CONTEXT.  It will be a stream for the socket
   on which the callback will arrive if a CALLBACK-CONTEXT was
   given. ID will be the notification ID used for this message."

  ;; sanity checks
  (required-string title)
  (required-string app)
  (required-string notification)
  (optional-id id)
  (unless (and (stringp body) (zerop (length body)))
    (optional-string body))
  (when priority
    (valid-priority priority))
  (optional-icon icon)
  (optional-id coalesce)
  (optional-string callback-context)
  (cond
    ((null callback-context) (optional-string callback-target))
    (t                       (required-string callback-context-type)))
  (required-string host)
  (valid-port port)

  (valid-encryption-checksum-combo encryption-mode checksum-mode)

  (unless (and (eql checksum-mode :none)
	       (eql encryption-mode :none))
    (required-string password))

  (when (and body (plusp (length body)))
    (format t "SENDING: ~A~%" body))

  (labels ((hdr (data-hash)
	     (hdr-line "Application-Name" app data-hash)
	     (hdr-line "Notification-Name" notification data-hash)

	     (when id
	       (hdr-line "Notification-ID" id data-hash))

	     (hdr-line "Notification-Title"
		       (or title *growl-default-title*) data-hash)
	     (when (and body (plusp (length body)))
	       (hdr-line "Notification-Text" body data-hash))

	     (when sticky
	       (hdr-line "Notification-Sticky" "True" data-hash))
	     (when priority
	       (hdr-line "Notification-Priority" priority data-hash))

	     (when icon
	       (hdr-line "Notification-Icon" icon data-hash))

	     (when coalesce
	       (hdr-line "Notification-Coalescing-ID" coalesce data-hash))

             (when callback-context
               (hdr-line "Notification-Callback-Context"
                         callback-context data-hash)
               (hdr-line "Notification-Callback-Context-Type"
                         callback-context-type data-hash))
             (when callback-target
               (hdr-line "Notification-Callback-Target"
                         callback-target data-hash))))
    (let* ((data-hash (make-hash-table))
	   (msg (with-output-to-binary-string
		  (compose +growl-notify-message-type+
			   :header (with-output-to-binary-string
				     (hdr data-hash))
			   :binary-data data-hash
			   :checksum-mode checksum-mode
			   :encryption-mode encryption-mode
			   :password password
			   :salt (string-to-utf-8-bytes "foobie")))))
      (let ((sock (usocket:socket-connect host port
					  :element-type '(unsigned-byte 8))))
	(unwind-protect
	     (progn
	       (write-sequence msg (usocket:socket-stream sock))
	       (force-output (usocket:socket-stream sock))
               (unless (and callback-context (not callback-target))
                 (echo-all-bytes (usocket:socket-stream sock))))
	  (unless (and callback-context (not callback-target))
	    (usocket:socket-close sock)))
	(values (when (and callback-context (not callback-target))
                  sock)
                id)))))
