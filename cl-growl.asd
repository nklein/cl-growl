(asdf:defsystem #:cl-growl
  :author "Patrick Stein <pat@nklein.com>"
  :maintainer "Patrick Stein <pat@nklein.com>"
  :description "Utilities for sending messages to Growl on Mac OS X."
  :license "Public Domain"
  :version "1.1.2013.02.26"
  :depends-on (#:trivial-utf-8 #:usocket-udp #:flexi-streams #:ironclad)
  :components ((:module "src"
		:components ((:file "package")
			     (:file "constants" :depends-on ("package"))
			     (:file "types"    :depends-on ("package"))
			     (:file "utils"    :depends-on ("package"))
			     (:file "specials" :depends-on ("package"
							    "types"
                                                            "utils"))
			     (:file "gntp"     :depends-on ("package"
							    "constants"
							    "types"
							    "utils"))
			     (:file "register" :depends-on ("package"
							    "constants"
							    "types"
							    "specials"
							    "utils"
							    "gntp"))
			     (:file "notify"   :depends-on ("package"
							    "constants"
							    "types"
							    "specials"
							    "utils"))))))
