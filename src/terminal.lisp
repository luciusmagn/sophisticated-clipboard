(in-package #:sophisticated-clipboard)

;;;; -- Terminal Backend (OSC 52) --

(defclass terminal-backend (clipboard-backend)
  ((writer
    :initarg :writer
    :reader terminal-backend-writer
    :type function
    :documentation "Called with each control string; returns true when a terminal received it.")
   (selection
    :initarg :selection
    :initform :clipboard
    :reader terminal-backend-selection
    :type (member :clipboard :primary)
    :documentation "The terminal selection the text is copied to."))
  (:default-initargs
   :writer (error "A terminal backend needs a :WRITER function."))
  (:documentation
   "Copy text through the controlling terminal with an OSC 52 request.

The terminal places the text on the clipboard of the machine the user sits
at, which is what reaches them over SSH or a session relay. The route only
writes: terminals do not report their clipboard back reliably."))

(defmethod clipboard-backend-name ((backend terminal-backend))
  "Name the terminal route."
  :terminal)

(defmethod backend-types ((backend terminal-backend))
  "Refuse to list types, since the terminal route cannot read."
  (error 'clipboard-unavailable
         :reason "the OSC 52 terminal route cannot read the clipboard"))

(defmethod backend-get ((backend terminal-backend) mime-type)
  "Refuse to read, since the terminal route cannot read."
  (declare (ignore mime-type))
  (backend-types backend))

(defmethod backend-set ((backend terminal-backend) data mime-type)
  "Send text DATA to the terminal, signalling CLIPBOARD-UNAVAILABLE when no terminal took it."
  (check-clipboard-data data mime-type)
  (unless (text-mime-type-p mime-type)
    (unsupported-type backend mime-type))
  (unless (funcall (terminal-backend-writer backend)
                   (terminal-clipboard-sequence
                    data
                    :selection (terminal-backend-selection backend)))
    (error 'clipboard-unavailable
           :reason "no terminal accepted the OSC 52 request"))
  data)

(defun terminal-clipboard-sequence (text &key (selection :clipboard))
  "Return the OSC 52 control asking a terminal to copy TEXT to SELECTION.

SELECTION is :CLIPBOARD or :PRIMARY. The payload is TEXT's UTF-8 encoding
in base64, and the control ends with ST."
  (check-type text string)
  (format nil "~C]52;~A;~A~C\\"
          (code-char 27)
          (ecase selection
            (:clipboard "c")
            (:primary "p"))
          (cl-base64:usb8-array-to-base64-string
           (flexi-streams:string-to-octets text :external-format :utf-8))
          (code-char 27)))
