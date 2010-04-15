(in-package :growl)

;;; ==========================================================
;;; Application-wide defaults
;;; ==========================================================
(defvar *growl-default-host* "localhost"
  "This is the default hostname of the Growl server")
(defvar *growl-default-port* 23053
  "This is the default port number the Growl server is listening on.
   This is the GNTP default port.")
(defvar *growl-default-app* "Common Lisp program"
  "This is the default name by which your application will identify
   itself to the Growl server.")
(defvar *growl-default-app-icon* "http://nklein.com/favicon.ico"
  "If not NIL, this icon will be transmitted to the Growl server.
   It can be either an encoded image stored in a one-dimensional
   binary array or a URL for the icon image to use.")

;;; ==========================================================
;;; Notification-specific defaults
;;; ==========================================================
(defvar *growl-default-notification* "info"
  "This is the default notification level to use when sending
   notification events to the Growl server.")

(defvar *growl-default-title* ""
  "This is the default message title to use when sending
   notification events to the Growl server.")
(defvar *growl-default-priority* nil
  "If not NIL, this is the default priority used when sending
   notification events to the Growl server.")
(defvar *growl-default-icon* nil
  "If not NIL, this is the default notification-icon that
   will be transmitted to the Growl server along with your
   notification events.  It can be either an encoded image
   stored in a one-dimensional binary array or a URL for the
   icon image to use.")

;;; ==========================================================
;;; Notification-callback defaults
;;; ==========================================================
(defvar *growl-default-callback-context* nil
  "If not NIL, this will be the default context supplied to
   a callback for the notification.  Growl invokes a callback
   when the user clicks the notification, when the user closes
   the notification, or when the notification times out.")
(defvar *growl-default-callback-context-type* nil
  "If not NIL, this specifies the context type information
   supplied to the callback for the notification.")

(defvar *growl-default-callback-target* nil
  "If not NIL, this should be a URL for a callback method.
   This URL will be opened in the user's default browser
   if the user clicks the notification.")

;;; ==========================================================
;;; Protocol-nitty-gritty defaults
;;; ==========================================================
(defvar *growl-default-origin-fields*
        `(("Machine-Name" ,(machine-instance))
	  ("Software-Name" "CL-GROWL")
	  ("Software-Version" "1.5")
	  ("Platform-Name" ,(concatenate 'string
					 (software-type)
					 " "
					 (machine-type)
					 " ("
					 (lisp-implementation-type)
					 " "
					 (lisp-implementation-version)
					 ")"))
	  ("Platform-Version" ,(software-version)))
  "GNTP allows fields that describe the system/software
   that generated the message.
   This must be a LIST of CONS cells.  The CAR of each
   cell is the field name.  The CDR of each cell is the
   field value.  The value can either be a string or
   a one-dimensional binary array.")
(defvar *growl-default-custom-fields* nil
  "GNTP allows custom fields in the Growl message headers.
   This must be a LIST of CONS cells.  The CAR of each
   cell is the field name.  The CDR of each cell is the
   field value.  The value can either be a string or
   a one-dimensional binary array.")
(defvar *growl-default-application-fields* nil
  "GNTP allows app-specific field in the Growl message headers.
   This must be a LIST of CONS cells.  The CAR of each
   cell is the field name.  The CDR of each cell is the
   field value.  The value can either be a string or
   a one-dimensional binary array.")

(defvar *growl-default-checksum-mode* #+ironclad :sha256
	                               #+md5 :md5
				       #-(or ironclad md5) :none
  "Default hash algorithm to use.  It must be a valid
   GROWL-CHECKSUM-MODE.")
(declaim (type growl-checksum-mode *growl-default-checksum-mode*))

(defvar *growl-default-encryption-mode* :none
  "Default encryption algorithm to use.  It could be :AES,
   :DES, :3DES, or :NONE.  You need Ironclad to do anything
   other than :NONE.")
(declaim (type growl-encryption-mode *growl-default-encryption-mode*))

(defvar *growl-default-password* ""
  "Default password to use if the hash algorithm is not :NONE")
