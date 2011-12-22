(require :asdf)
(asdf:operate 'asdf:load-op 'ironclad)
(asdf:operate 'asdf:load-op 'cl-growl)

;;;
;;; need IronClad for SHA-256, can use
;;; IronClad or MD5 for MD5 checksum
;;; can use :NONE for no checksum if local
;;;
;;; need IronClad for AES, DES, or 3DES encryption-modes.
;;; can use :NONE for no encryption if local
(growl:register
   :app "Lambda Fun"
   :app-icon "http://nklein.com/favicon.ico"
   :enabled '( "info" "warn" "error" )
   :disabled '( "debug" )
   :host "localhost"
   :port 23053
   :password "growl-password"
   :checksum-mode :sha256
   :encryption-mode :aes)

;;; Prepare some defaults
(setf growl:*growl-default-app* "Lambda Fun"
      growl:*growl-default-host* "localhost"
      growl:*growl-default-encryption-mode* :aes
      growl:*growl-default-password* "growl-password"
      growl:*growl-default-notification* "info")

;;; Send some messages
(growl:notify "Here is a message.")

(growl:notify "This message has a title"
	      :title "You will note:")

(growl:notify "This message sticks"
	      :sticky t)

(growl:notify "This is an error!!"
	      :notification "error")

(growl:notify "Go to the CL-Growl Web Page!"
	      :title "Do not pass GO"
	      :sticky t
	      :callback-target "http://nklein.com/software/cl-growl/")
