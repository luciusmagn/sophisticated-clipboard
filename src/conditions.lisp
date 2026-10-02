(in-package #:sophisticated-clipboard)

;;;; -- Conditions --

(define-condition sophisticated-clipboard-error (error)
  ()
  (:documentation "The parent of every condition this library signals."))

(define-condition clipboard-unavailable (sophisticated-clipboard-error)
  ((reason
    :initarg :reason
    :initform "no clipboard backend applies to this process"
    :reader clipboard-unavailable-reason))
  (:report (lambda (condition stream)
             (format stream "No system clipboard is reachable: ~A."
                     (clipboard-unavailable-reason condition))))
  (:documentation "No backend can reach a clipboard from this process."))

(define-condition not-installed (clipboard-unavailable)
  ((programs
    :initarg :programs
    :initform nil
    :reader not-installed-programs))
  (:report (lambda (condition stream)
             (format stream "None of the clipboard commands are installed: ~{~A~^, ~}."
                     (not-installed-programs condition))))
  (:documentation "A display is present but none of its clipboard commands are installed."))

(define-condition clipboard-command-failed (sophisticated-clipboard-error)
  ((command
    :initarg :command
    :reader clipboard-command-failed-command)
   (exit-code
    :initarg :exit-code
    :reader clipboard-command-failed-exit-code)
   (output
    :initarg :output
    :initform ""
    :reader clipboard-command-failed-output))
  (:report (lambda (condition stream)
             (format stream "Clipboard command ~{~A~^ ~} exited with ~A~@[: ~A~]"
                     (clipboard-command-failed-command condition)
                     (clipboard-command-failed-exit-code condition)
                     (let ((output (clipboard-command-failed-output condition)))
                       (and (plusp (length output))
                            (string-trim '(#\Newline #\Return #\Space) output))))))
  (:documentation "A clipboard helper command exited unsuccessfully."))

(define-condition clipboard-unsupported-type (sophisticated-clipboard-error)
  ((mime-type
    :initarg :mime-type
    :reader clipboard-unsupported-type-mime-type)
   (backend
    :initarg :backend
    :reader clipboard-unsupported-type-backend))
  (:report (lambda (condition stream)
             (format stream "The ~A clipboard backend cannot transfer ~A."
                     (clipboard-unsupported-type-backend condition)
                     (clipboard-unsupported-type-mime-type condition))))
  (:documentation "The selected backend cannot carry data of the requested type."))
