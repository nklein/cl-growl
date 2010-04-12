(in-package :growl)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +growl-protocol-version+ 1 "GROWL Protocol Version")

#+ironclad
(define-constant +growl-protocol-version-aes128+ 2
                 "GROWL Protocol Version with Encryption")

#+(or md5 ironclad)
(define-constant +growl-type-registration+ 0 "Registration packet with MD5")

#+(or md5 ironclad)
(define-constant +growl-type-notification+ 1 "Notification packet with MD5")

#+ironclad
(define-constant +growl-type-registration-sha256+ 2
                 "Registration packet with SHA-256 checksum")
#+ironclad
(define-constant +growl-type-notification-sha256+ 3
                 "Notification packet with SHA-256 checksum")

(define-constant +growl-type-registration-noauth+ 4
                 "Registration packet without authentication")
(define-constant +growl-type-notification-noauth+ 5
                 "Notification packet without authentication")
