(asdf:defsystem #:growl
  :depends-on (#:trivial-utf-8 #:usocket-udp #:flexi-streams)
  :weakly-depends-on (#:md5 #:ironclad)
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
