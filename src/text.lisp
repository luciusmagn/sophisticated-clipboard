(in-package :sophisticated-clipboard)

(defun executable-find (command)
  "Search for COMMAND in the PATH and return the absolute file name.
Return nil if COMMAND is not found anywhere."
  (multiple-value-bind (path)
      (ignore-errors
       (uiop:run-program (format nil "command -v ~A" command)
                         :output '(:string :stripped t)))
    path))

(defun wayland-session-p ()
  (string= (uiop:getenvp "XDG_SESSION_TYPE") "wayland"))

(defun x-session-p ()
  (find (uiop:getenvp "XDG_SESSION_TYPE") '("x11" "tty") :test #'string=))

;;; Platform-specific implementations
#-os-windows
(defmethod clipboard-types ()
  (cond
    ;; On Windows, we do windows
    #+os-windows
    ((uiop:os-windows-p)
     (call-next-method))
    ;; Try X11
    ((and (uiop:getenvp "DISPLAY") (executable-find "xclip"))
     (let ((output (uiop:run-program
                    '("xclip" "-selection" "clipboard" "-target" "TARGETS" "-out")
                    :output '(:string :stripped t)
                    :ignore-error-status t)))
       (when output
         (mapcar #'make-clipboard-type
                 (remove-if (lambda (s) (or (string= s "TARGETS")
                                            (string= s "")))
                            (uiop:split-string output :separator '(#\Newline)))))))
    ;; Then try Wayland
    ((wayland-session-p)
     (let ((output (uiop:run-program '("wl-paste" "--list-types")
                                     :output '(:string :stripped t)
                                     :ignore-error-status t)))
       (when output
         (princ output)
         (mapcar #'make-clipboard-type
                 (uiop:split-string output :separator '(#\Newline))))))
    (t nil)))

#-os-windows
(defmethod clipboard-has-type-p ((mime-type string))
  #+os-windows
  (if (uiop:os-windows-p)
      (call-next-method)
      (find mime-type (clipboard-types) :key #'mime-type :test #'string=))
  #-os-windows
  (find mime-type (clipboard-types) :key #'mime-type :test #'string=))

#-os-windows
(defmethod clipboard-get ((mime-type string))
  (cond
    #+os-windows
    ((uiop:os-windows-p)
     (call-next-method))
    ((wayland-session-p)
     (clipboard-get-wayland mime-type))
    ((x-session-p)
     (clipboard-get-x11 mime-type))
    (t (error "Unsupported platform"))))

#-os-windows
(defmethod clipboard-set (data (mime-type string))
  (cond
    #+os-windows
    ((uiop:os-windows-p)
     (call-next-method))
    ((wayland-session-p)
     (clipboard-set-wayland data mime-type))
    ((x-session-p)
     (clipboard-set-x11 data mime-type))
    (t (error "Unsupported platform"))))

;;; Wayland implementation
(defun clipboard-get-wayland (mime-type)
  (let ((is-text (or (search "text/" mime-type)
                     (member mime-type '("STRING" "TEXT" "UTF8_STRING" "COMPOUND_TEXT")
                             :test #'string=))))
    (if is-text
        (uiop:run-program `("wl-paste" "--type" ,mime-type)
                          :output '(:string :stripped t))
        ;; Binary data
        (with-output-to-sequence (stream :element-type '(unsigned-byte 8))
          (uiop:run-program `("wl-paste" "--type" ,mime-type)
                            :output stream)))))


(defun clipboard-set-wayland (data mime-type)
  (let ((is-text (or (stringp data)
                     (and (search "text/" mime-type))
                     (member mime-type '("STRING" "TEXT" "UTF8_STRING" "COMPOUND_TEXT")
                             :test #'string=))))
    (if is-text
        (with-input-from-string (input (if (stringp data) data (error "Non-string data for text type")))
          (uiop:run-program `("wl-copy" "--type" ,mime-type)
                            :input input))
        ;; Binary data
        (with-input-from-sequence (input data)
          (uiop:run-program `("wl-copy" "--type" ,mime-type)
                            :input input)))))


;;; X11 implementation

(defun clipboard-get-x11 (mime-type)
  (let ((is-text (search "text/" mime-type)))
    (if is-text
        (uiop:run-program `("xclip" "-selection" "clipboard" "-target" ,mime-type "-out")
                          :output '(:string :stripped t))
        (with-output-to-sequence (stream :element-type '(unsigned-byte 8))
          (uiop:run-program `("xclip" "-selection" "clipboard" "-target" ,mime-type "-out")
                            :output stream)))))

(defun clipboard-set-x11 (data mime-type)
  (let ((is-text (stringp data)))
    (if is-text
        (with-input-from-string (input data)
          (uiop:run-program `("xclip" "-selection" "clipboard" "-target" ,mime-type "-in")
                            :input input))
        (with-input-from-sequence (input data)
          (uiop:run-program `("xclip" "-selection" "clipboard" "-target" ,mime-type "-in")
                            :input input)))))

;;; High-level convenience API

(defun clipboard-text ()
  "Get text from clipboard, trying common text formats"
  (loop for mime in '("text/plain;charset=utf-8" "text/plain" "UTF8_STRING" "STRING")
        when (clipboard-has-type-p mime)
          return (clipboard-get mime)))

(defun (setf clipboard-text) (text)
  "Set clipboard text"
  (clipboard-set text "text/plain;charset=utf-8")
  text)

(defun clipboard-image (&optional (preferred-type "image/png"))
  "Get image from clipboard as byte array"
  (let ((image-types (remove-if-not (lambda (type)
                                      (eq (type-category type) :image))
                                    (clipboard-types))))
    (when image-types
      (clipboard-get (or (find preferred-type image-types
                               :key #'mime-type :test #'string=)
                         (mime-type (first image-types)))))))

(defun (setf clipboard-image) (data &optional (mime-type "image/png"))
  "Set clipboard image from byte array"
  (clipboard-set data mime-type)
  data)
