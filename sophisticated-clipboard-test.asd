(in-package :asdf-user)
(defsystem sophisticated-clipboard-test
  :author "SANO Masatoshi"
  :depends-on ("sophisticated-clipboard" "fiveam")
  :components ((:file "test"))
  :perform (test-op (o s)
                    (symbol-call :fiveam :run! :sophisticated-clipboard)))
