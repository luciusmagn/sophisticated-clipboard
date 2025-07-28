(in-package :sophisticated-clipboard)

(defclass clipboard-type ()
  ((mime-type :initarg :mime-type :reader mime-type)
   (name :initarg :name :reader type-name)
   (category :initarg :category :reader type-category))
  (:documentation "Represents a clipboard data type"))

(defmethod print-object ((type clipboard-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~A" (mime-type type))))

(defun make-clipboard-type (type-string)
  "Create clipboard-type from MIME type or X11 atom"
  (let* ((slash-pos (position #\/ type-string))
         (category (cond
                     ;; X11 atoms (no slash)
                     ((member type-string '("STRING" "TEXT" "UTF8_STRING"
                                            "COMPOUND_TEXT" "text")
                              :test #'string=) :text)
                     ((member type-string '("PIXMAP" "BITMAP") :test #'string=) :image)
                     ;; MIME types (has slash)
                     ((and slash-pos (search "image/" type-string)) :image)
                     ((and slash-pos (search "text/" type-string)) :text)
                     ((and slash-pos (search "audio/" type-string)) :audio)
                     ((and slash-pos (search "video/" type-string)) :video)
                     ((and slash-pos (search "application/" type-string)) :application)
                     (t :other)))
         (name (cond
                 ;; X11 atoms - use as-is
                 ((not slash-pos) type-string)
                 ;; MIME types - extract subtype after slash
                 (t (subseq type-string (1+ slash-pos))))))
    (make-instance 'clipboard-type
                   :mime-type type-string
                   :name name
                   :category category)))

(defgeneric clipboard-types ()
  (:documentation "Return list of clipboard-type objects available in clipboard"))

(defgeneric clipboard-has-type-p (mime-type)
  (:documentation "Check if clipboard contains data of given MIME type"))

(defgeneric clipboard-get (mime-type)
  (:documentation "Get clipboard content as specific MIME type. Returns byte array for binary data, string for text."))

(defgeneric clipboard-set (data mime-type)
  (:documentation "Set clipboard content with specific MIME type"))
