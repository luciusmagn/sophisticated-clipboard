(in-package #:sophisticated-clipboard)

;;;; -- Clipboard Access --

(defun clipboard-types (&optional (backend (clipboard-backend)))
  "Return the CLIPBOARD-TYPE objects the clipboard offers through BACKEND."
  (backend-types backend))

(defun clipboard-has-type-p (mime-type &optional (backend (clipboard-backend)))
  "Return true when the clipboard offers MIME-TYPE through BACKEND."
  (not (null (find-clipboard-type mime-type (backend-types backend)))))

(defun clipboard-get (mime-type &optional (backend (clipboard-backend)))
  "Return the clipboard content as MIME-TYPE: a string for text, octets otherwise."
  (backend-get backend mime-type))

(defun clipboard-set (data mime-type &optional (backend (clipboard-backend)))
  "Replace the clipboard content with DATA labelled MIME-TYPE and return DATA."
  (backend-set backend data mime-type)
  data)

(defun clipboard-text (&optional (backend (clipboard-backend)))
  "Return the clipboard's plain text, or NIL when it holds none."
  (backend-text backend))

(defun (setf clipboard-text) (text &optional (backend (clipboard-backend)))
  "Replace the clipboard content with plain TEXT."
  (setf (backend-text backend) text))

(defun clipboard-image (&optional (preferred-type "image/png")
                                  (backend (clipboard-backend)))
  "Return the clipboard image as octets, preferring PREFERRED-TYPE, or NIL."
  (let ((image-types (remove :image (backend-types backend)
                             :key #'type-category :test-not #'eq)))
    (when image-types
      (backend-get backend
                   (if (find-clipboard-type preferred-type image-types)
                       preferred-type
                       (mime-type (first image-types)))))))

(defun (setf clipboard-image) (data &optional (mime-type "image/png")
                                              (backend (clipboard-backend)))
  "Replace the clipboard content with image DATA of MIME-TYPE."
  (backend-set backend data mime-type)
  data)
