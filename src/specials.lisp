(in-package :growl)

(defvar *growl-default-host* "localhost")
(defvar *growl-default-port* 9887)
(defvar *growl-default-app* "Common Lisp program")
(defvar *growl-default-enabled-notifications*
        (list "Common Lisp notification")
    "List of Growl notifications that are enabled by default")
(defvar *growl-default-disabled-notifications*
        (list)
    "List of Growl notifications that are disabled by default")

(defvar *growl-default-title* "")
(defvar *growl-default-password* "")
