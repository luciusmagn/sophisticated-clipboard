(in-package #:sophisticated-clipboard)

;;;; -- Wayland --

(defclass wayland-backend (clipboard-backend)
  ()
  (:documentation "The Wayland clipboard through wl-copy and wl-paste from wl-clipboard."))

(defmethod clipboard-backend-name ((backend wayland-backend))
  "Name the Wayland backend."
  :wayland)

(defmethod backend-types ((backend wayland-backend))
  "List the offered MIME types; an empty clipboard makes wl-paste fail."
  (mapcar #'make-clipboard-type
          (command-lines
           (run-text-command '("wl-paste" "--list-types") :failure-value ""))))

(defmethod backend-get ((backend wayland-backend) mime-type)
  "Read MIME-TYPE through wl-paste, without the newline it would append to text."
  (if (text-mime-type-p mime-type)
      (run-text-command (list "wl-paste" "--no-newline" "--type" mime-type)
                        :failure-value nil)
      (run-octet-command (list "wl-paste" "--type" mime-type)
                         :failure-value nil)))

(defmethod backend-set ((backend wayland-backend) data mime-type)
  "Offer DATA as MIME-TYPE through wl-copy."
  (check-clipboard-data data mime-type)
  (run-feeding-command (list "wl-copy" "--type" mime-type) data))


;;;; -- X11 --

(defparameter *x11-meta-targets*
  '("TARGETS" "MULTIPLE" "TIMESTAMP" "SAVE_TARGETS")
  "Selection targets that describe the selection rather than carry content.")

(defclass x11-backend (clipboard-backend)
  ((tool
    :initarg :tool
    :initform :xclip
    :reader x11-backend-tool
    :type (member :xclip :xsel)
    :documentation "The command driving the X11 CLIPBOARD selection."))
  (:documentation "The X11 CLIPBOARD selection through xclip, or text only through xsel."))

(defmethod clipboard-backend-name ((backend x11-backend))
  "Name the X11 backend."
  :x11)

(defmethod print-object ((backend x11-backend) stream)
  (print-unreadable-object (backend stream :type t)
    (format stream "~A via ~(~A~)" (clipboard-backend-name backend)
            (x11-backend-tool backend))))

(defmethod backend-types ((backend x11-backend))
  "List the selection targets; xsel can only reveal whether text is present."
  (ecase (x11-backend-tool backend)
    (:xclip
     (mapcar #'make-clipboard-type
             (set-difference
              (command-lines
               (run-text-command '("xclip" "-selection" "clipboard"
                                   "-target" "TARGETS" "-out")
                                 :failure-value ""))
              *x11-meta-targets*
              :test #'string=)))
    (:xsel
     (let ((text (run-text-command '("xsel" "--clipboard" "--output")
                                   :failure-value "")))
       (and (plusp (length text))
            (mapcar #'make-clipboard-type
                    '("UTF8_STRING" "text/plain;charset=utf-8" "text/plain")))))))

(defmethod backend-get ((backend x11-backend) mime-type)
  "Read MIME-TYPE from the CLIPBOARD selection."
  (ecase (x11-backend-tool backend)
    (:xclip
     (let ((command (list "xclip" "-selection" "clipboard" "-target" mime-type "-out")))
       (if (text-mime-type-p mime-type)
           (run-text-command command :failure-value nil)
           (run-octet-command command :failure-value nil))))
    (:xsel
     (unless (text-mime-type-p mime-type)
       (unsupported-type backend mime-type))
     (run-text-command '("xsel" "--clipboard" "--output") :failure-value nil))))

(defmethod backend-set ((backend x11-backend) data mime-type)
  "Own the CLIPBOARD selection with DATA as MIME-TYPE."
  (check-clipboard-data data mime-type)
  (ecase (x11-backend-tool backend)
    (:xclip
     (run-feeding-command
      (list "xclip" "-selection" "clipboard" "-target" mime-type "-in")
      data))
    (:xsel
     (unless (text-mime-type-p mime-type)
       (unsupported-type backend mime-type))
     (run-feeding-command '("xsel" "--clipboard" "--input") data))))
