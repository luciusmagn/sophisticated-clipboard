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

(defun clipboard-copy-text (text &key (backend *clipboard-backend*) terminal-writer)
  "Copy TEXT to the host clipboard and, given TERMINAL-WRITER, through the terminal.

The host clipboard goes through BACKEND, detected when NIL. A TERMINAL-WRITER
also receives an OSC 52 request through a TERMINAL-BACKEND, because over SSH
or a session relay the host clipboard belongs to the wrong machine and only
the terminal reaches the user's own. A terminal that ignores the request
loses nothing.

Returns three values: whether the host clipboard took TEXT, whether a
terminal took it, and the host's SOPHISTICATED-CLIPBOARD-ERROR or NIL."
  (check-type text string)
  (multiple-value-bind (host-p host-condition)
      (handler-case
          (progn
            (setf (backend-text (or backend (clipboard-detect-backend))) text)
            (values t nil))
        (sophisticated-clipboard-error (condition)
          (values nil condition)))
    (values host-p
            (and terminal-writer
                 (handler-case
                     (progn
                       (setf (backend-text
                              (make-instance 'terminal-backend :writer terminal-writer))
                             text)
                       t)
                   (clipboard-unavailable ()
                     nil)))
            host-condition)))
