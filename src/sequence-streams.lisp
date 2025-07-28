(in-package :sophisticated-clipboard)

(defmacro with-output-to-sequence ((var &key (element-type ''(unsigned-byte 8)))
                                   &body body)
  "Execute BODY with VAR bound to a stream that collects output into a sequence"
  `(let ((,var (flexi-streams:make-in-memory-output-stream
                :element-type ,element-type)))
     ,@body
     (flexi-streams:get-output-stream-sequence ,var)))

(defmacro with-input-from-sequence ((var sequence) &body body)
  "Execute BODY with VAR bound to a stream reading from SEQUENCE"
  `(let ((,var (flexi-streams:make-in-memory-input-stream ,sequence)))
     ,@body))
