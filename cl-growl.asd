;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(asdf:defsystem #:cl-growl
  :depends-on (#:trivial-utf-8 #:usocket #:flexi-streams)
  :weakly-depends-on (#:md5 #:ironclad)
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
