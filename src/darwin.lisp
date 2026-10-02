(in-package #:sophisticated-clipboard)

;;;; -- macOS Pasteboard --

(defclass darwin-backend (clipboard-backend)
  ()
  (:documentation
   "The macOS general pasteboard through pbcopy, pbpaste, and osascript.

Text travels through pbcopy and pbpaste. Images travel through AppleScript
coercions of the clipboard to and from a temporary file, so no compiled
bridge is needed."))

(defmethod clipboard-backend-name ((backend darwin-backend))
  "Name the macOS backend."
  :darwin)

(defparameter *darwin-class-mime-types*
  '(("«class utf8»" . "text/plain;charset=utf-8")
    ("«class ut16»" . "text/plain;charset=utf-8")
    ("string" . "text/plain;charset=utf-8")
    ("Unicode text" . "text/plain;charset=utf-8")
    ("«class PNGf»" . "image/png")
    ("«class TIFF»" . "image/tiff")
    ("TIFF picture" . "image/tiff")
    ("«class JPEG»" . "image/jpeg")
    ("JPEG picture" . "image/jpeg")
    ("«class HTML»" . "text/html")
    ("«class RTF »" . "text/rtf")
    ("«class furl»" . "text/uri-list")
    ("«class url »" . "text/uri-list"))
  "AppleScript clipboard classes and the MIME types they stand for.")

(defparameter *darwin-image-classes*
  '(("image/png" . "«class PNGf»")
    ("image/tiff" . "«class TIFF»")
    ("image/jpeg" . "«class JPEG»"))
  "Image MIME types this backend transfers and their AppleScript classes.")

(defun darwin-parse-clipboard-info (output)
  "Return the distinct MIME types named by the `clipboard info` OUTPUT.

The output lists class and byte-size pairs separated by commas. Classes
without a known MIME type are returned by their AppleScript name."
  (let ((items (mapcar (lambda (item)
                         (string-trim '(#\Space #\Newline #\Return) item))
                       (uiop:split-string output :separator ",")))
        (types nil))
    (loop for (class size) on items by #'cddr
          when (and class (plusp (length class)) size)
            do (let ((mime (or (cdr (assoc class *darwin-class-mime-types*
                                           :test #'string=))
                               class)))
                 (pushnew mime types :test #'string=)))
    (nreverse types)))

(defun darwin-run-applescript (&rest lines)
  "Run the AppleScript LINES through osascript and return its output."
  (run-text-command
   (cons "osascript"
         (loop for line in lines
               append (list "-e" line)))))

(defun darwin-image-class (backend mime-type)
  "Return the AppleScript class for image MIME-TYPE or signal unsupported."
  (or (cdr (assoc mime-type *darwin-image-classes* :test #'string=))
      (unsupported-type backend mime-type)))

(defmethod backend-types ((backend darwin-backend))
  "List the pasteboard's types through AppleScript's `clipboard info`."
  (mapcar #'make-clipboard-type
          (darwin-parse-clipboard-info
           (darwin-run-applescript "clipboard info"))))

(defmethod backend-get ((backend darwin-backend) mime-type)
  "Read text through pbpaste, or an image through an AppleScript coercion."
  (cond
    ((text-mime-type-p mime-type)
     (let ((text (run-text-command '("pbpaste") :failure-value nil)))
       (and text (plusp (length text)) text)))
    (t
     (let ((class (darwin-image-class backend mime-type)))
       (uiop:with-temporary-file (:pathname pathname :type "clipboard" :keep nil)
         (let ((native (uiop:native-namestring pathname)))
           (handler-case
               (darwin-run-applescript
                (format nil "set f to open for access POSIX file ~S with write permission"
                        native)
                "set eof of f to 0"
                (format nil "write (the clipboard as ~A) to f" class)
                "close access f")
             (clipboard-command-failed ()
               (return-from backend-get nil)))
           (with-open-file (stream pathname :element-type '(unsigned-byte 8))
             (let ((octets (make-array (file-length stream)
                                       :element-type '(unsigned-byte 8))))
               (read-sequence octets stream)
               (and (plusp (length octets)) octets)))))))))

(defmethod backend-set ((backend darwin-backend) data mime-type)
  "Write text through pbcopy, or an image through an AppleScript coercion."
  (check-clipboard-data data mime-type)
  (cond
    ((text-mime-type-p mime-type)
     (run-feeding-command '("pbcopy") data))
    (t
     (let ((class (darwin-image-class backend mime-type)))
       (uiop:with-temporary-file (:pathname pathname :type "clipboard" :keep nil)
         (with-open-file (stream pathname
                                 :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
           (write-sequence data stream))
         (darwin-run-applescript
          (format nil "set the clipboard to (read (POSIX file ~S) as ~A)"
                  (uiop:native-namestring pathname) class))
         data)))))
