(asdf:defsystem #:cl-growl
  :version "1.1.2013.02.26"
  :depends-on (#:trivial-utf-8 #:usocket-udp #:flexi-streams #:ironclad)
  :components ((:module "src"
		:components ((:file "package")
			     (:file "constants" :depends-on ("package"))
			     (:file "specials" :depends-on ("package"))
			     (:file "utils"    :depends-on ("package"))
			     (:file "register" :depends-on ("package"
							    "constants"
							    "specials"
							    "utils"))
			     (:file "notify"   :depends-on ("package"
							    "constants"
							    "specials"
							    "utils"))))))
