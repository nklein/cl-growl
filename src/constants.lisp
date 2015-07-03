(in-package :growl)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +growl-protocol-version+ "1.0"
  "GNTP Protocol Version")

(define-constant +growl-register-message-type+ "REGISTER"
  "Message type for register messages.")

(define-constant +growl-notify-message-type+ "NOTIFY"
  "Message type for notify messages.")

(define-constant +growl-subscribe-message-type+ "SUBSCRIBE"
  "Message type for subscribe messages.")
