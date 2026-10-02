(in-package #:sophisticated-clipboard)

;;;; -- Backend Protocol --

(defvar *clipboard-backend* nil
  "The backend used when a call names none, or NIL to detect one per call.

Bind or set this to pin a backend, for example to keep using X11 under a
Wayland session, or to install a test double.")

(defclass clipboard-backend ()
  ()
  (:documentation "One way of reaching a system clipboard."))

(defgeneric clipboard-backend-name (backend)
  (:documentation "Return the keyword naming BACKEND, such as :WAYLAND or :X11."))

(defgeneric backend-types (backend)
  (:documentation "Return the CLIPBOARD-TYPE objects BACKEND's clipboard offers now."))

(defgeneric backend-get (backend mime-type)
  (:documentation
   "Return the clipboard content as MIME-TYPE through BACKEND.

Text types return a string and every other type returns an octet vector.
The result is NIL when the clipboard holds nothing of that type."))

(defgeneric backend-set (backend data mime-type)
  (:documentation
   "Replace the clipboard content with DATA labelled MIME-TYPE through BACKEND.

DATA is a string for text types and an octet vector otherwise."))

(defgeneric backend-text (backend)
  (:documentation "Return the clipboard's plain text through BACKEND, or NIL."))

(defgeneric (setf backend-text) (text backend)
  (:documentation "Replace the clipboard content with plain TEXT through BACKEND."))

(defparameter *text-mime-type-preferences*
  '("text/plain;charset=utf-8" "UTF8_STRING" "text/plain" "STRING" "TEXT")
  "Text types to request, most specific first, when reading plain text.")

(defmethod backend-text ((backend clipboard-backend))
  "Read plain text through the first offered type in *TEXT-MIME-TYPE-PREFERENCES*."
  (let ((types (backend-types backend)))
    (loop for candidate in *text-mime-type-preferences*
          when (find-clipboard-type candidate types)
            return (backend-get backend candidate))))

(defmethod (setf backend-text) (text (backend clipboard-backend))
  "Write plain TEXT as UTF-8 text/plain."
  (check-type text string)
  (backend-set backend text "text/plain;charset=utf-8")
  text)

(defmethod print-object ((backend clipboard-backend) stream)
  (print-unreadable-object (backend stream :type t)
    (format stream "~A" (clipboard-backend-name backend))))

(defun check-clipboard-data (data mime-type)
  "Signal a TYPE-ERROR unless DATA suits MIME-TYPE: a string for text, octets otherwise."
  (if (text-mime-type-p mime-type)
      (check-type data string)
      (check-type data (vector (unsigned-byte 8))))
  data)

(defun unsupported-type (backend mime-type)
  "Signal that BACKEND cannot transfer MIME-TYPE."
  (error 'clipboard-unsupported-type
         :backend (clipboard-backend-name backend)
         :mime-type mime-type))
