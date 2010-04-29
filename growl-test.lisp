(require :asdf)
(asdf:operate 'asdf:load-op 'ironclad)
(asdf:operate 'asdf:load-op 'cl-growl)

;;; need IronClad for SHA-256, can use
;;; IronClad or MD5 for MD5 checksum
;;; can use “none” for no checksum if local
(growl:register
   :app "Lambda Fun"
   :enabled '( "info" "warn" "error" )
   :disabled '( "debug" )
   :host "localhost"
   :checksum-mode :sha256
   :password "growl-password")

;;; Prepare some defaults
(setf growl:*growl-default-app* "Lambda Fun"
      growl:*growl-default-host* "localhost"
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
   