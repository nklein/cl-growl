(in-package :growl)

(defun notify (message &key (title *growl-default-title*)
	               (app *growl-default-app*)
	               (notification (first *growl-default-enabled-notifications*))
	               (priority 0)
	               (sticky nil)
		       (host *growl-default-host*)
		       (port *growl-default-port*)
		       (checksum-mode (or #+ironclad :sha256
					  #+md5      :md5
				                     :noauth))
		       (password *growl-default-password*)
		       #+ironclad (encryptp nil)
		  &aux (message-enc (string-to-utf-8-bytes message))
	               (title-enc (string-to-utf-8-bytes title))
	               (app-enc (string-to-utf-8-bytes app))
	               (notification-enc (string-to-utf-8-bytes notification)))
  "Make a notification of type NOTIFICATION for the app APP with description MESSAGE."

        ;; version is unencrypted unless we have the means to encrypt
        ;; (read: ironclad and a password)
  (let ((ver (or #+ironclad (when encryptp +growl-protocol-version-aes128+)
		 +growl-protocol-version+))
	(type (ecase checksum-mode
		#+ironclad (:sha256 +growl-type-notification-sha256+)
		#+(or md5 ironclad) (:md5 +growl-type-notification+)
		(:noauth +growl-type-registration-noauth+)))
	(notification-len (length notification-enc))
	(title-len (length title-enc))
	(message-len (length message-enc))
	(app-len (length app-enc))

	;; Note: the protocol claims that the priority is on the
	;; range [-2,2] and is kept as a 3-bit quantity along with
	;; a sticky big in the lowest nibble of the flags.  In all
	;; of the sample code, the sticky bit is kept in the lowest
	;; bit of the first byte of the flags and the priority is
	;; kept in the upper three bits of the lowest nibble of the
	;; second byte of the flags.  It seems that they had intended
	;; to put the sticky bit in the bottom bit of that lowest
	;; nibble, but accidently put it in the bottom bit of the
	;; third lowest nibble, and the rest is history.
	;;
	;; Also, most implementations must mask the priority with 0x07,
	;; multiply it by two, and then explicitly turn on the sign
	;; bit when the priority is negative.  This is redundant and
	;; lets in quantities like +3 and -4.  So, I have been more
	;; explicit in my calculation.  Of course, the receiving side
	;; totally ignores the priority in the notification and goes
	;; with the priority set in the preferences for the notification
	;; type.
	(flags   (logior (* (logand (max (min priority 2) -2) #x03) 2)
			 (if (< priority 0) #x08 #x00)
			 (if sticky #x0100 #x0000))))
    (let ((stream (flexi-streams:make-in-memory-output-stream)))
      (write-byte ver stream)
      (write-byte type stream)
      (write-short flags stream)
      (write-short notification-len stream)
      (write-short title-len stream)
      (write-short message-len stream)
      (write-short app-len stream)
      (write-sequence notification-enc stream)
      (write-sequence title-enc stream)
      (write-sequence message-enc stream)
      (write-sequence app-enc stream)

      (send-packet (flexi-streams:get-output-stream-sequence stream)
		   :host host :port port
		   :checksum-mode checksum-mode
		   :password password
		   #+ironclad :encryptp #+ironclad encryptp))))
