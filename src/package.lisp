(defpackage #:cl-growl
  (:use "COMMON-LISP" "TRIVIAL-UTF-8")
  (:nicknames #:growl)
  (:export #:register
	   #:notify
	   #:*growl-default-host*
	   #:*growl-default-port*
	   #:*growl-default-app*
	   #:*growl-default-app-icon*
	   #:*growl-default-notification*
	   #:*growl-default-title*
	   #:*growl-default-priority*
	   #:*growl-default-icon*
           #:*growl-default-salt*
	   #:*growl-default-callback-context*
	   #:*growl-default-callback-context-type*
	   #:*growl-default-callback-target*
	   #:*growl-default-origin-fields*
	   #:*growl-default-custom-fields*
	   #:*growl-default-checksum-mode*
	   #:*growl-default-encryption-mode*
	   #:*growl-default-password*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :ironclad)
    (pushnew :ironclad *features*))
  #-ironclad
  (when (find-package :md5)
    (pushnew :md5 *features*)))
