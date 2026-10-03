(defpackage #:sophisticated-clipboard/tests
  (:use #:cl #:fiveam)
  (:import-from #:sophisticated-clipboard
                #:clipboard-available-p
                #:backend-get
                #:backend-set
                #:backend-text
                #:backend-types
                #:clipboard-backend
                #:clipboard-backend-name
                #:clipboard-copy-text
                #:clipboard-unsupported-type
                #:sophisticated-clipboard-error
                #:terminal-backend
                #:terminal-clipboard-sequence
                #:clipboard-detect-backend
                #:clipboard-text
                #:clipboard-unavailable
                #:darwin-backend
                #:darwin-parse-clipboard-info
                #:executable-find
                #:make-clipboard-type
                #:mime-type
                #:not-installed
                #:not-installed-programs
                #:text-mime-type-p
                #:type-category
                #:type-name
                #:wayland-backend
                #:x11-backend
                #:x11-backend-tool))

(in-package #:sophisticated-clipboard/tests)

(def-suite :sophisticated-clipboard
  :description "Backend detection, type classification, and live round trips.")

(in-suite :sophisticated-clipboard)

(defun environment-function (bindings)
  "Return a GETENV replacement answering from the alist BINDINGS."
  (lambda (name)
    (cdr (assoc name bindings :test #'string=))))

(defun executables-function (names)
  "Return an EXECUTABLE-FIND replacement that only finds NAMES."
  (lambda (command)
    (and (member command names :test #'string=)
         (pathname command))))

(defun detect (operating-system bindings names)
  "Detect a backend for OPERATING-SYSTEM with fake environment and executables."
  (clipboard-detect-backend
   :operating-system operating-system
   :getenv (environment-function bindings)
   :executable-find (executables-function names)))

(test type-classification
  (let ((cases '(("text/plain;charset=utf-8" :text "plain;charset=utf-8")
                 ("UTF8_STRING" :text "UTF8_STRING")
                 ("image/png" :image "png")
                 ("audio/ogg" :audio "ogg")
                 ("video/mp4" :video "mp4")
                 ("application/pdf" :application "pdf")
                 ("TIMESTAMP" :other "TIMESTAMP")
                 ("x-special/gnome-copied-files" :other "gnome-copied-files"))))
    (loop for (string category name) in cases
          do (let ((type (make-clipboard-type string)))
               (is (string= string (mime-type type)))
               (is (eq category (type-category type)))
               (is (string= name (type-name type))))))
  (is-true (text-mime-type-p "text/html"))
  (is-true (text-mime-type-p "STRING"))
  (is-false (text-mime-type-p "image/png"))
  (is-false (text-mime-type-p "textual")))

(test posix-detection
  (let ((wayland '(("WAYLAND_DISPLAY" . "wayland-0") ("DISPLAY" . ":0")))
        (x11 '(("DISPLAY" . ":0")))
        (nothing '(("WAYLAND_DISPLAY" . ""))))
    (is (typep (detect :posix wayland '("wl-copy" "wl-paste" "xclip")) 'wayland-backend))
    (let ((backend (detect :posix wayland '("xclip"))))
      (is (typep backend 'x11-backend))
      (is (eq :xclip (x11-backend-tool backend))))
    (let ((backend (detect :posix x11 '("xsel"))))
      (is (typep backend 'x11-backend))
      (is (eq :xsel (x11-backend-tool backend))))
    (is (eq :xclip (x11-backend-tool (detect :posix x11 '("xsel" "xclip")))))
    (signals not-installed (detect :posix x11 '("wl-copy" "wl-paste")))
    (signals clipboard-unavailable (detect :posix nothing '("xclip" "wl-copy")))
    (handler-case (detect :posix '(("WAYLAND_DISPLAY" . "wayland-1")) '())
      (not-installed (condition)
        (is (equal '("wl-copy" "wl-paste") (not-installed-programs condition)))))))

(test darwin-detection
  (is (typep (detect :darwin '() '("pbcopy" "pbpaste")) 'darwin-backend))
  (signals not-installed (detect :darwin '() '("pbcopy"))))

(test darwin-clipboard-info
  (is (equal '("text/plain;charset=utf-8" "image/png")
             (darwin-parse-clipboard-info
              "«class utf8», 5, «class ut16», 10, string, 5, «class PNGf», 1234")))
  (is (equal '("image/tiff" "«class weir»")
             (darwin-parse-clipboard-info "«class TIFF», 12, «class weir», 3")))
  (is (null (darwin-parse-clipboard-info ""))))

(test executable-lookup
  (is (null (executable-find "sophisticated-clipboard-missing-command")))
  (is (null (executable-find "sh" :path "")))
  (unless (uiop:os-windows-p)
    (is (not (null (executable-find "sh"))))))

(defparameter *round-trip-samples*
  (list " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
        (format nil "日本語~%汉语~%اللغة العربية~%русский язык")
        "😀😁😂 💪♐🌵 🇦🆗⬇"
        (format nil "CR~ACR+LF~A~ALF~AOK?"
                (code-char #x0d) (code-char #x0d) (code-char #x0a) (code-char #x0a)))
  "Texts that must survive a clipboard round trip unchanged.")

(test live-round-trip
  (if (clipboard-available-p)
      (dolist (sample *round-trip-samples*)
        (setf (clipboard-text) sample)
        (is (string= sample (clipboard-text))))
      (skip "no clipboard backend is reachable from this process")))

(test live-octet-round-trip
  (if (clipboard-available-p)
      (let ((octets (make-array 300 :element-type '(unsigned-byte 8)))
            (backend (sophisticated-clipboard:clipboard-detect-backend)))
        (dotimes (index 300)
          (setf (aref octets index) (mod (* index 7) 256)))
        (if (and (typep backend 'x11-backend)
                 (eq :xsel (x11-backend-tool backend)))
            (skip "xsel carries text only")
            (progn
              (sophisticated-clipboard:clipboard-set octets "application/octet-stream")
              (is (equalp octets
                          (sophisticated-clipboard:clipboard-get
                           "application/octet-stream"))))))
      (skip "no clipboard backend is reachable from this process")))

(test rejects-wrong-data
  (if (clipboard-available-p)
      (signals type-error (setf (clipboard-text) 1))
      (skip "no clipboard backend is reachable from this process")))

(defun osc-52 (selection payload)
  "Return the exact OSC 52 ST control for SELECTION and base64 PAYLOAD."
  (format nil "~C]52;~A;~A~C\\" (code-char 27) selection payload (code-char 27)))

(defclass recording-backend (clipboard-backend)
  ((texts :initform nil :accessor recording-backend-texts)
   (fail-p :initarg :fail-p :initform nil :reader recording-backend-fail-p))
  (:documentation "A host backend recording copies, or refusing every one."))

(defmethod clipboard-backend-name ((backend recording-backend))
  :recording)

(defmethod backend-types ((backend recording-backend))
  nil)

(defmethod backend-get ((backend recording-backend) mime-type)
  (declare (ignore mime-type))
  nil)

(defmethod backend-set ((backend recording-backend) data mime-type)
  (declare (ignore mime-type))
  (when (recording-backend-fail-p backend)
    (error 'clipboard-unavailable :reason "refused for the test"))
  (push data (recording-backend-texts backend)))

(test terminal-sequence
  (is (string= (osc-52 "c" "aGk=") (terminal-clipboard-sequence "hi")))
  (is (string= (osc-52 "p" "aGk=") (terminal-clipboard-sequence "hi" :selection :primary)))
  (is (string= (osc-52 "c" "4pyTCuKclw==")
               (terminal-clipboard-sequence (format nil "~C~%~C"
                                                    (code-char #x2713)
                                                    (code-char #x2717)))))
  (signals type-error (terminal-clipboard-sequence 1)))

(test terminal-backend
  (let* ((written '())
         (backend (make-instance 'terminal-backend
                                 :writer (lambda (control) (push control written) t))))
    (is (eq :terminal (clipboard-backend-name backend)))
    (setf (backend-text backend) "hi")
    (is (equal (list (osc-52 "c" "aGk=")) written))
    (signals clipboard-unsupported-type
      (backend-set backend (make-array 1 :element-type '(unsigned-byte 8)) "image/png"))
    (signals clipboard-unavailable (backend-types backend))
    (signals clipboard-unavailable (backend-text backend)))
  (signals clipboard-unavailable
    (setf (backend-text (make-instance 'terminal-backend :writer (constantly nil))) "hi"))
  (signals error (make-instance 'terminal-backend)))

(test copy-text
  (let ((host (make-instance 'recording-backend))
        (written '()))
    (multiple-value-bind (host-p terminal-p condition)
        (clipboard-copy-text "hi"
                             :backend host
                             :terminal-writer (lambda (control) (push control written) t))
      (is-true host-p)
      (is-true terminal-p)
      (is (null condition))
      (is (equal '("hi") (recording-backend-texts host)))
      (is (equal (list (osc-52 "c" "aGk=")) written))))
  (multiple-value-bind (host-p terminal-p condition)
      (clipboard-copy-text "hi"
                           :backend (make-instance 'recording-backend :fail-p t)
                           :terminal-writer (constantly t))
    (is-false host-p)
    (is (eq t terminal-p))
    (is (typep condition 'sophisticated-clipboard-error)))
  (multiple-value-bind (host-p terminal-p condition)
      (clipboard-copy-text "hi"
                           :backend (make-instance 'recording-backend :fail-p t)
                           :terminal-writer (constantly nil))
    (is-false host-p)
    (is-false terminal-p)
    (is (typep condition 'clipboard-unavailable)))
  (multiple-value-bind (host-p terminal-p)
      (clipboard-copy-text "hi" :backend (make-instance 'recording-backend))
    (is-true host-p)
    (is-false terminal-p)))
