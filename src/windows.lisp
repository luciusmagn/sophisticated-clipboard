(in-package #:sophisticated-clipboard)

;;;; -- Windows Clipboard --

(cffi:define-foreign-library user32
  (t (:default "user32")))

(cffi:use-foreign-library user32)

(defparameter *win32-cf-text* 1
  "CF_TEXT, ANSI text.")

(defparameter *win32-cf-unicodetext* 13
  "CF_UNICODETEXT, UTF-16LE text with a terminating null.")

(defparameter *win32-cf-dib* 8
  "CF_DIB, a device-independent bitmap without a file header.")

(defparameter *win32-cf-dibv5* 17
  "CF_DIBV5, a version 5 device-independent bitmap.")

(defparameter *win32-gmem-flags* #x2002
  "GMEM_MOVEABLE and GMEM_DDESHARE, the allocation flags for clipboard memory.")

(defparameter *win32-registered-format-names*
  '(("image/png" . "PNG")
    ("text/html" . "HTML Format")
    ("image/dib" . :dib)
    ("image/dibv5" . :dibv5))
  "MIME types and the registered or predefined Windows formats carrying them.")

(defclass windows-backend (clipboard-backend)
  ()
  (:documentation "The Windows clipboard through the Win32 API."))

(defmethod clipboard-backend-name ((backend windows-backend))
  "Name the Windows backend."
  :windows)

(defun win32-call-with-open-clipboard (function)
  "Call FUNCTION while this process owns the open clipboard, retrying briefly.

Another application may hold the clipboard open for a moment, so OpenClipboard
is retried a few times before signalling CLIPBOARD-UNAVAILABLE."
  (loop repeat 10
        do (when (cffi:foreign-funcall "OpenClipboard"
                                       :pointer (cffi:null-pointer)
                                       :boolean)
             (return-from win32-call-with-open-clipboard
               (unwind-protect
                    (funcall function)
                 (cffi:foreign-funcall "CloseClipboard" :boolean))))
           (sleep 0.01))
  (error 'clipboard-unavailable :reason "the Windows clipboard could not be opened"))

(defun win32-register-format (name)
  "Return the format identifier of the registered clipboard format NAME."
  (cffi:foreign-funcall "RegisterClipboardFormatA"
                        :string name
                        :unsigned-int))

(defun win32-format-identifier (mime-type)
  "Return the Windows clipboard format carrying MIME-TYPE."
  (let ((entry (cdr (assoc mime-type *win32-registered-format-names*
                           :test #'string=))))
    (cond
      ((text-mime-type-p mime-type) *win32-cf-unicodetext*)
      ((eq entry :dib) *win32-cf-dib*)
      ((eq entry :dibv5) *win32-cf-dibv5*)
      ((stringp entry) (win32-register-format entry))
      (t (win32-register-format mime-type)))))

(defun win32-format-name (identifier)
  "Return the MIME type or registered name for clipboard format IDENTIFIER, or NIL."
  (cond
    ((= identifier *win32-cf-unicodetext*) "text/plain;charset=utf-8")
    ((= identifier *win32-cf-text*) "text/plain")
    ((= identifier *win32-cf-dib*) "image/dib")
    ((= identifier *win32-cf-dibv5*) "image/dibv5")
    (t
     (cffi:with-foreign-object (buffer :char 256)
       (let ((length (cffi:foreign-funcall "GetClipboardFormatNameA"
                                           :unsigned-int identifier
                                           :pointer buffer
                                           :int 256
                                           :int)))
         (when (plusp length)
           (let ((name (cffi:foreign-string-to-lisp buffer :count length)))
             (or (car (rassoc name *win32-registered-format-names* :test #'equal))
                 name))))))))

(defmethod backend-types ((backend windows-backend))
  "Enumerate the clipboard's formats as MIME types where they are known."
  (win32-call-with-open-clipboard
   (lambda ()
     (let ((types nil))
       (loop with format = 0
             do (setf format (cffi:foreign-funcall "EnumClipboardFormats"
                                                   :unsigned-int format
                                                   :unsigned-int))
             until (zerop format)
             do (let ((name (win32-format-name format)))
                  (when name
                    (pushnew (make-clipboard-type name) types
                             :key #'mime-type :test #'string=))))
       (nreverse types)))))

(defun win32-global-octets (handle)
  "Copy the global memory block HANDLE into an octet vector."
  (let ((pointer (cffi:foreign-funcall "GlobalLock" :pointer handle :pointer)))
    (when (cffi:null-pointer-p pointer)
      (error 'clipboard-unavailable :reason "GlobalLock failed on clipboard memory"))
    (unwind-protect
         (let* ((size (cffi:foreign-funcall "GlobalSize"
                                            :pointer handle
                                            :unsigned-long-long))
                (octets (make-array size :element-type '(unsigned-byte 8))))
           (dotimes (index size octets)
             (setf (aref octets index) (cffi:mem-ref pointer :uint8 index))))
      (cffi:foreign-funcall "GlobalUnlock" :pointer handle :boolean))))

(defun win32-global-text (handle)
  "Decode the null-terminated UTF-16LE text in global memory HANDLE."
  (let ((pointer (cffi:foreign-funcall "GlobalLock" :pointer handle :pointer)))
    (when (cffi:null-pointer-p pointer)
      (error 'clipboard-unavailable :reason "GlobalLock failed on clipboard text"))
    (unwind-protect
         (cffi:foreign-string-to-lisp pointer :encoding :utf-16le)
      (cffi:foreign-funcall "GlobalUnlock" :pointer handle :boolean))))

(defmethod backend-get ((backend windows-backend) mime-type)
  "Read MIME-TYPE as UTF-16 text or as the raw bytes of its format."
  (let ((format (win32-format-identifier mime-type)))
    (win32-call-with-open-clipboard
     (lambda ()
       (let ((handle (cffi:foreign-funcall "GetClipboardData"
                                           :unsigned-int format
                                           :pointer)))
         (unless (cffi:null-pointer-p handle)
           (if (text-mime-type-p mime-type)
               (win32-global-text handle)
               (win32-global-octets handle))))))))

(defun win32-allocate-global (size)
  "Allocate SIZE bytes of moveable global memory and return its handle."
  (let ((handle (cffi:foreign-funcall "GlobalAlloc"
                                      :unsigned-int *win32-gmem-flags*
                                      :unsigned-long-long size
                                      :pointer)))
    (when (cffi:null-pointer-p handle)
      (error 'clipboard-unavailable :reason "GlobalAlloc failed for clipboard memory"))
    handle))

(defun win32-fill-global (handle source size)
  "Copy SIZE bytes from foreign SOURCE into global memory HANDLE."
  (let ((pointer (cffi:foreign-funcall "GlobalLock" :pointer handle :pointer)))
    (when (cffi:null-pointer-p pointer)
      (error 'clipboard-unavailable :reason "GlobalLock failed on new clipboard memory"))
    (unwind-protect
         (dotimes (index size)
           (setf (cffi:mem-ref pointer :uint8 index)
                 (cffi:mem-ref source :uint8 index)))
      (cffi:foreign-funcall "GlobalUnlock" :pointer handle :boolean))))

(defun win32-publish (format handle)
  "Make HANDLE the clipboard's FORMAT content, freeing it when the system refuses."
  (win32-call-with-open-clipboard
   (lambda ()
     (cffi:foreign-funcall "EmptyClipboard" :boolean)
     (when (cffi:null-pointer-p
            (cffi:foreign-funcall "SetClipboardData"
                                  :unsigned-int format
                                  :pointer handle
                                  :pointer))
       (cffi:foreign-funcall "GlobalFree" :pointer handle :pointer)
       (error 'clipboard-unavailable :reason "SetClipboardData failed")))))

(defmethod backend-set ((backend windows-backend) data mime-type)
  "Write text as CF_UNICODETEXT, or octets under the format carrying MIME-TYPE."
  (check-clipboard-data data mime-type)
  (let ((format (win32-format-identifier mime-type)))
    (if (stringp data)
        (multiple-value-bind (source size)
            (cffi:foreign-string-alloc data :encoding :utf-16le)
          (unwind-protect
               (let ((handle (win32-allocate-global size)))
                 (win32-fill-global handle source size)
                 (win32-publish format handle))
            (cffi:foreign-string-free source)))
        (let ((size (length data)))
          (cffi:with-foreign-object (source :uint8 (max 1 size))
            (dotimes (index size)
              (setf (cffi:mem-ref source :uint8 index) (aref data index)))
            (let ((handle (win32-allocate-global (max 1 size))))
              (win32-fill-global handle source size)
              (win32-publish format handle)))))
    data))
