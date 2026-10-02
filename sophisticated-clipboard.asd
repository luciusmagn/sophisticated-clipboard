(in-package :asdf-user)

(defsystem "sophisticated-clipboard"
  :version "0.1.0"
  :author "Lukáš Hozda, SANO Masatoshi"
  :maintainer "Lukáš Hozda <me@mag.wiki>"
  :description "Portable system clipboard access for Common Lisp"
  :license "MIT"
  :depends-on ("uiop"
               "flexi-streams"
               #+os-windows "cffi")
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "conditions")
                 (:file "types")
                 (:file "sequence-streams")
                 (:file "executables")
                 (:file "backend")
                 (:file "commands")
                 (:file "posix")
                 (:file "darwin")
                 #+os-windows (:file "windows")
                 (:file "detection")
                 (:file "api"))))
  :in-order-to ((test-op (test-op "sophisticated-clipboard/tests"))))

(defsystem "sophisticated-clipboard/tests"
  :depends-on ("sophisticated-clipboard" "fiveam")
  :components ((:module "tests"
                :components ((:file "tests"))))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (symbol-call :fiveam :run! :sophisticated-clipboard)
               (error "sophisticated-clipboard tests failed."))))
