(in-package :asdf-user)
(defsystem sophisticated-clipboard
  :version "0.0.0.0"
  :author "Lukáš Hozda, SANO Masatoshi"
  :maintainer "Lukáš Hozda <me@mag.wiki>"
  :description "A Common Lisp library for accessing system clipboards"
  :license "MIT"
  :depends-on ("uiop"
               "flexi-streams"
               #+os-windows "cffi"
               #+darwin "cfii")
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "conditions")
                 (:file "types")
                 (:file "sequence-streams")
                 #+os-windows (:file "windows")
                 #+darwin (:file "darwin")
                 (:file "text"))))
  :in-order-to ((test-op (test-op sophisticated-clipboard-test))))
