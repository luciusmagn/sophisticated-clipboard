(in-package :sophisticated-clipboard)

#+darwin
(progn
  (define-foreign-library clipboard-lib
      (t (:default "src/darwin/clipboard")))

  (use-foreign-library clipboard-lib)

  (defcfun "getClipboardTypes" :pointer (count (:pointer :int)))
  (defcfun "freeClipboardTypes" :void (types :pointer) (count :int))
  (defcfun "hasClipboardType" :int (type :string))
  (defcfun "getClipboardData" :pointer (type :string) (length (:pointer :int)))
  (defcfun "setClipboardData" :int (type :string) (data :pointer) (length :int))
  (defcfun "resetClipboard" :void)

  (defmethod clipboard-types ()
    (with-foreign-object (count :int)
      (let ((types-ptr (getClipboardTypes count)))
        (when (not (null-pointer-p types-ptr))
          (unwind-protect
               (loop for i from 0 below (mem-ref count :int)
                     collect (make-clipboard-type
                              (mem-ref (mem-aref types-ptr :pointer i) :string)))
            (freeClipboardTypes types-ptr (mem-ref count :int)))))))

  (defmethod clipboard-has-type-p ((mime-type string))
    (= 1 (hasClipboardType mime-type)))

  (defmethod clipboard-get ((mime-type string))
    (with-foreign-object (length :int)
      (let ((data-ptr (getClipboardData mime-type length)))
        (when (not (null-pointer-p data-ptr))
          (let* ((len (mem-ref length :int))
                 (is-text (or (search "text/" mime-type)
                              (string= mime-type "public.utf8-plain-text"))))
            (unwind-protect
                 (if is-text
                     (foreign-string-to-lisp data-ptr :count len :encoding :utf-8)
                     (let ((result (make-array len :element-type '(unsigned-byte 8))))
                       (loop for i from 0 below len do
                         (setf (aref result i) (mem-ref data-ptr :unsigned-char i)))
                       result))
              (foreign-free data-ptr)))))))

  (defmethod clipboard-set (data (mime-type string))
    (resetClipboard)
    (if (stringp data)
        (with-foreign-string (str data)
          (= 1 (setClipboardData mime-type str (length data))))
        ;; Binary data
        (with-foreign-object (arr :unsigned-char (length data))
          (loop for i from 0 below (length data) do
            (setf (mem-ref arr :unsigned-char i) (aref data i)))
          (= 1 (setClipboardData mime-type arr (length data)))))))
