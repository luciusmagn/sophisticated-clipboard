(in-package :sophisticated-clipboard)

(define-condition sophisticated-clipboard-error (simple-error) ())

(define-condition not-installed (sophisticated-clipboard-error)
  ((programs
    :initarg :programs
    :reader not-installed-programs))
  (:report (lambda (c s)
             (format s "None of the commands are installed: ~S"
                     (not-installed-programs c)))))
