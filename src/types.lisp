(in-package #:sophisticated-clipboard)

;;;; -- Clipboard Data Types --

(defclass clipboard-type ()
  ((mime-type
    :initarg :mime-type
    :reader mime-type
    :documentation "The MIME type or selection target name as the clipboard reports it.")
   (name
    :initarg :name
    :reader type-name
    :documentation "The MIME subtype, or the whole name for X11 selection targets.")
   (category
    :initarg :category
    :reader type-category
    :documentation "One of :TEXT, :IMAGE, :AUDIO, :VIDEO, :APPLICATION, or :OTHER."))
  (:documentation "One data type currently offered by a clipboard."))

(defmethod print-object ((type clipboard-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~A" (mime-type type))))

(defparameter *x11-text-targets*
  '("STRING" "TEXT" "UTF8_STRING" "COMPOUND_TEXT" "text")
  "X11 selection targets that carry plain text.")

(defun text-mime-type-p (type-string)
  "Return true when TYPE-STRING names plain or structured text."
  (or (member type-string *x11-text-targets* :test #'string=)
      (and (>= (length type-string) 5)
           (string= "text/" type-string :end2 5))))

(defun make-clipboard-type (type-string)
  "Return a CLIPBOARD-TYPE describing MIME type or X11 target TYPE-STRING."
  (let* ((slash (position #\/ type-string))
         (category
           (cond
             ((text-mime-type-p type-string) :text)
             ((member type-string '("PIXMAP" "BITMAP") :test #'string=) :image)
             ((null slash) :other)
             ((string= "image/" type-string :end2 (min 6 (length type-string))) :image)
             ((string= "audio/" type-string :end2 (min 6 (length type-string))) :audio)
             ((string= "video/" type-string :end2 (min 6 (length type-string))) :video)
             ((string= "application/" type-string
                       :end2 (min 12 (length type-string)))
              :application)
             (t :other)))
         (name (if slash
                   (subseq type-string (1+ slash))
                   type-string)))
    (make-instance 'clipboard-type
                   :mime-type type-string
                   :name name
                   :category category)))

(defun find-clipboard-type (mime-type types)
  "Return the CLIPBOARD-TYPE in TYPES whose MIME type is MIME-TYPE, or NIL."
  (find mime-type types :key #'mime-type :test #'string=))
