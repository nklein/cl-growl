(defpackage #:cl-growl
  (:use "COMMON-LISP" "TRIVIAL-UTF-8")
  (:nicknames #:growl)
  (:export #:register
	   #:notify
	   #:*growl-default-host*
	   #:*growl-default-port*
	   #:*growl-default-app*
	   #:*growl-default-enabled-notifications*
	   #:*growl-default-disabled-notifications*
	   #:*growl-default-title*
	   #:*growl-default-password*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :ironclad)
    (pushnew :ironclad *features*))
  #-ironclad
  (when (find-package :md5)
    (pushnew :md5 *features*)))
