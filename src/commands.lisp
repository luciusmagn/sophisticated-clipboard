(in-package #:sophisticated-clipboard)

;;;; -- Running Helper Commands --

(defun run-text-command (command &key input (failure-value nil failure-value-p))
  "Run COMMAND and return its standard output as a UTF-8 string.

INPUT, when given, is written to the command as UTF-8. A failing exit signals
CLIPBOARD-COMMAND-FAILED unless FAILURE-VALUE is supplied, which is then
returned instead; clipboard tools exit unsuccessfully for an empty clipboard."
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program command
                        :input (and input (make-string-input-stream input))
                        :output '(:string)
                        :error-output '(:string)
                        :external-format :utf-8
                        :ignore-error-status t)
    (cond
      ((zerop exit-code)
       output)
      (failure-value-p
       failure-value)
      (t
       (error 'clipboard-command-failed
              :command command
              :exit-code exit-code
              :output error-output)))))

(defun run-octet-command (command &key input (failure-value nil failure-value-p))
  "Run COMMAND and return its standard output as an octet vector.

INPUT, when given, is an octet vector written to the command. Failure handling
matches RUN-TEXT-COMMAND."
  (with-output-to-sequence (output)
    (multiple-value-bind (ignored-output error-output exit-code)
        (if input
            (with-input-from-sequence (input-stream input)
              (uiop:run-program command
                                :input input-stream
                                :output output
                                :error-output '(:string)
                                :element-type '(unsigned-byte 8)
                                :ignore-error-status t))
            (uiop:run-program command
                              :output output
                              :error-output '(:string)
                              :element-type '(unsigned-byte 8)
                              :ignore-error-status t))
      (declare (ignore ignored-output))
      (unless (zerop exit-code)
        (if failure-value-p
            (return-from run-octet-command failure-value)
            (error 'clipboard-command-failed
                   :command command
                   :exit-code exit-code
                   :output (if (stringp error-output) error-output "")))))))

(defun run-feeding-command (command data)
  "Run COMMAND with DATA on its standard input, discarding its output.

Selection owners such as wl-copy, xclip, and xsel fork a background process
that keeps serving the clipboard; capturing their output would wait for that
child, so only the exit status is observed. DATA is a string or octet vector."
  (let ((exit-code
          (if (stringp data)
              (nth-value 2
                         (uiop:run-program command
                                           :input (make-string-input-stream data)
                                           :output nil
                                           :error-output nil
                                           :external-format :utf-8
                                           :ignore-error-status t))
              (with-input-from-sequence (input data)
                (nth-value 2
                           (uiop:run-program command
                                             :input input
                                             :output nil
                                             :error-output nil
                                             :element-type '(unsigned-byte 8)
                                             :ignore-error-status t))))))
    (unless (zerop exit-code)
      (error 'clipboard-command-failed :command command :exit-code exit-code))
    data))

(defun command-lines (output)
  "Split command OUTPUT into its non-empty lines."
  (remove "" (uiop:split-string output :separator '(#\Newline #\Return))
          :test #'string=))
