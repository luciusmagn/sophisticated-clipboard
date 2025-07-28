;;;
;;; Windows support using Win32 API
;;;

(cl:in-package :sophisticated-clipboard)

#+os-windows
(progn
  (define-foreign-library user32
      (t (:default "user32")))

  (use-foreign-library user32)

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defconstant +win32-cf-unicodetext+ 13)
    (defconstant +win32-cf-text+ 1)
    (defconstant +win32-cf-bitmap+ 2)
    (defconstant +win32-cf-dib+ 8)
    (defconstant +win32-cf-html+ (RegisterClipboardFormat "HTML Format"))
    (defconstant +win32-gmem-moveable+ 2)
    (defconstant +win32-gmem-ddeshare+ 8192))

  (defcfun "RegisterClipboardFormat" :uint (format :string))
  (defcfun "CountClipboardFormats" :int)
  (defcfun "EnumClipboardFormats" :uint (format :uint))
  (defcfun "GetClipboardFormatName" :int (format :uint) (name :pointer) (max-count :int))

  (defmethod clipboard-types ()
    (when (foreign-funcall "OpenClipboard" :pointer (null-pointer) :boolean)
      (unwind-protect
           (let ((formats '()))
             (loop with format = 0
                   while (not (zerop (setf format (EnumClipboardFormats format))))
                   do (let ((name (format-id-to-name format)))
                        (when name
                          (push (make-clipboard-type name) formats))))
             (nreverse formats))
        (foreign-funcall "CloseClipboard" :boolean))))

  (defun format-id-to-name (format-id)
    "Convert Windows clipboard format ID to name"
    (case format-id
      (#.+win32-cf-unicodetext+ "text/plain;charset=utf-16")
      (#.+win32-cf-text+ "text/plain")
      (#.+win32-cf-bitmap+ "image/bitmap")
      (#.+win32-cf-dib+ "image/dib")
      (otherwise
       (with-foreign-object (name :char 256)
         (when (> (GetClipboardFormatName format-id name 256) 0)
           (foreign-string-to-lisp name))))))

  (defun name-to-format-id (name)
    "Convert name to Windows clipboard format ID"
    (cond
      ((string= name "text/plain;charset=utf-16") +win32-cf-unicodetext+)
      ((string= name "text/plain") +win32-cf-text+)
      ((string= name "image/bitmap") +win32-cf-bitmap+)
      ((string= name "image/dib") +win32-cf-dib+)
      (t (RegisterClipboardFormat name))))

  (defmethod clipboard-has-type-p ((mime-type string))
    (when (foreign-funcall "OpenClipboard" :pointer (null-pointer) :boolean)
      (unwind-protect
           (not (null-pointer-p
                 (foreign-funcall "GetClipboardData"
                                  :uint (name-to-format-id mime-type)
                                  :pointer)))
        (foreign-funcall "CloseClipboard" :boolean))))

  (defmethod clipboard-get ((mime-type string))
    (if (string= mime-type "text/plain;charset=utf-16")
        (get-text-on-win32)  ; Use existing optimized text function
        (get-binary-data-win32 mime-type)))

  (defmethod clipboard-set (data (mime-type string))
    (if (and (stringp data) (string= mime-type "text/plain;charset=utf-16"))
        (set-text-on-win32 data)  ; Use existing optimized text function
        (set-binary-data-win32 data mime-type)))

  (defun get-binary-data-win32 (mime-type)
    (when (foreign-funcall "OpenClipboard" :pointer (null-pointer) :boolean)
      (unwind-protect
           (let ((hmem (foreign-funcall "GetClipboardData"
                                        :uint (name-to-format-id mime-type)
                                        :pointer)))
             (when (not (null-pointer-p hmem))
               (let* ((data-ptr (foreign-funcall "GlobalLock" :pointer hmem :pointer))
                      (size (foreign-funcall "GlobalSize" :pointer hmem :uint)))
                 (unwind-protect
                      (let ((result (make-array size :element-type '(unsigned-byte 8))))
                        (loop for i from 0 below size do
                          (setf (aref result i) (mem-ref data-ptr :unsigned-char i)))
                        result)
                   (foreign-funcall "GlobalUnlock" :pointer hmem)))))
        (foreign-funcall "CloseClipboard" :boolean))))

  (defun set-binary-data-win32 (data mime-type)
    (let ((size (if (stringp data)
                    (* 2 (1+ (length data)))  ; UTF-16 + null terminator
                    (length data))))
      (let ((hmem (foreign-funcall "GlobalAlloc"
                                   :uint #.(logior +win32-gmem-ddeshare+ +win32-gmem-moveable+)
                                   :uint size
                                   :pointer)))
        (when (null-pointer-p hmem)
          (error "GlobalAlloc failed"))
        (handler-bind
            ((error (lambda (c)
                      (declare (ignore c))
                      (foreign-funcall "GlobalFree" :pointer hmem :pointer))))
          (let ((data-ptr (foreign-funcall "GlobalLock" :pointer hmem :pointer)))
            (unwind-protect
                 (if (stringp data)
                     (lisp-string-to-foreign data data-ptr size :encoding :utf-16le)
                     (loop for i from 0 below (length data) do
                       (setf (mem-ref data-ptr :unsigned-char i) (aref data i))))
              (foreign-funcall "GlobalUnlock" :pointer hmem)))
          (when (foreign-funcall "OpenClipboard" :pointer (null-pointer) :boolean)
            (unwind-protect
                 (progn
                   (foreign-funcall "EmptyClipboard" :boolean)
                   (foreign-funcall "SetClipboardData"
                                    :uint (name-to-format-id mime-type)
                                    :pointer hmem
                                    :pointer))
              (foreign-funcall "CloseClipboard" :boolean))))))))
