(in-package :growl)

(deftype growl-checksum-mode ()
  "If you have Ironclad, you can do :SHA256, :MD5, or :NONE.
   If you have the MD5 package, you can do :MD5 or :NONE.
   If you have neither Ironclad nor MD5, you can only do :NONE.
   With :NONE, you will not be able to do authenticated Growl
   transactions."
  `(member :none
	   #+(or ironclad md5) :md5
	   #+ironclad :sha256))

(deftype growl-binary-data-type ()
  `(array (unsigned-byte 8) (*)))

(deftype growl-encryption-mode ()
  "If you have Ironclad, then you can encrypt with mode :AES,
   :DES, :3DES, or :NONE.  If you do not have Ironclad, you
   can only use the :NONE mode.  You can still do authenticated
   Growl transactions with encryption mode :NONE, but the
   contents of the transaction will go over the network in
   plain text so an eavesdropper could potentially read the
   contents of the Growl."
  `(member :none
	   #+ironclad :aes
	   #+ironclad :des
	   #+ironclad :3des))

(defun report-unavailable-checksum-error (err stream)
  (let ((mode (slot-value err 'requested-mode)))
    (case mode
      (:sha256 (format stream "Need IRONCLAD package to SHA256 checksum"))
      (:md5    (format stream "Need IRONCLAD or MD5 package to MD5 checksum"))
      (otherwise (format stream "Unknown checksum mode: ~S" mode)))))

(define-condition unavailable-checksum-error (error)
  ((requested-mode :initarg :requested-mode
		   :reader unavailable-checksum-requested-mode))
  (:report report-unavailable-checksum-error))

(defun report-unavailable-encryption-error (err stream)
  (let ((mode (slot-value err 'requested-mode)))
    (case mode
      (:aes  (format stream "Need IRONCLAD package to AES encrypt"))
      (:des  (format stream "Need IRONCLAD package to DES encrypt"))
      (:3des (format stream "Need IRONCLAD package to 3DES encrypt"))
      (otherwise (format stream "Unknown encryption mode: ~S" mode)))))

(define-condition unavailable-encryption-error (error)
  ((requested-mode :initarg :requested-mode
		   :reader unavailable-encryption-requested-mode))
  (:report report-unavailable-encryption-error))
