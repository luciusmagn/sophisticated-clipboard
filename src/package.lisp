(cl:in-package :cl-user)

(defpackage sophisticated-clipboard
  (:use :cl #+os-windows :cffi)
  (:export
   ;; New type-aware API
   :clipboard-type
   :mime-type
   :type-name
   :type-category
   :clipboard-types
   :clipboard-has-type-p
   :clipboard-get
   :clipboard-set

   ;; Convenience functions
   :clipboard-text
   :clipboard-image

   ;; Conditions
   :sophisticated-clipboard-error
   :not-installed))
