(cl:in-package :cl-user)

(defpackage sophisticated-clipboard
  (:use :cl #+os-windows :cffi)
  (:export :text :content))
