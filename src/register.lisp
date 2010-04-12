(in-package :growl)

(defun register (&key (app *growl-default-app*)
		      (enabled *growl-default-enabled-notifications*)
		      (disabled *growl-default-disabled-notifications*)
		      (host *growl-default-host*)
		      (port *growl-default-port*)
		      (checksum-mode (or #+ironclad :sha256
					 #+md5      :md5
				                    :noauth))
		      (password *growl-default-password*)
		      #+(and ironclad notyet) (encryptp nil)
		 &aux (app-enc (string-to-utf-8-bytes app)))
  "Register as the application named APP with the ENABLED notifications turned on and the DISABLED notifications turned off by default.  If PASSWORD is given, then use AES128 on the message."

  (let ((ver (or #+(and ironclad notyet)
		 (when encryptp +growl-protocol-version-aes128+)
		 +growl-protocol-version+))
	(type (ecase checksum-mode
		#+ironclad (:sha256 +growl-type-registration-sha256+)
		#+(or md5 ironclad) (:md5 +growl-type-registration+)
		(:noauth +growl-type-registration-noauth+)))
	(app-len (length app-enc))
	(nall (+ (length enabled) (length disabled)))
	(ndef (length enabled)))
    (let ((stream (flexi-streams:make-in-memory-output-stream)))
      (write-byte ver stream)
      (write-byte type stream)
      (write-short app-len stream)
      (write-byte nall stream)
      (write-byte ndef stream)
      (write-sequence app-enc stream)
      (loop :for nn :in (append enabled disabled)
	 :do (let ((enc (string-to-utf-8-bytes nn)))
	       (write-short (length enc) stream)
	       (write-sequence enc stream)))
      (loop :for ii :from 0 :below ndef
	 :do (write-byte ii stream))

      (send-packet (flexi-streams:get-output-stream-sequence stream)
		   :host host :port port
		   :checksum-mode checksum-mode
		   :password password
		   #+(and ironclad notyet) :encryptp
		   #+(and ironclad notyet) encryptp))))
