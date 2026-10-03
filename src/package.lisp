(defpackage #:sophisticated-clipboard
  (:use #:cl)
  (:export
   ;; Clipboard data types
   #:clipboard-type
   #:make-clipboard-type
   #:mime-type
   #:type-name
   #:type-category
   #:text-mime-type-p

   ;; Backends
   #:*clipboard-backend*
   #:clipboard-backend
   #:clipboard-backend-name
   #:clipboard-detect-backend
   #:clipboard-available-p
   #:wayland-backend
   #:x11-backend
   #:x11-backend-tool
   #:darwin-backend
   #:windows-backend
   #:terminal-backend
   #:terminal-backend-writer
   #:terminal-backend-selection
   #:terminal-clipboard-sequence
   #:backend-types
   #:backend-get
   #:backend-set
   #:backend-text

   ;; Clipboard access
   #:clipboard-types
   #:clipboard-has-type-p
   #:clipboard-get
   #:clipboard-set
   #:clipboard-text
   #:clipboard-image
   #:clipboard-copy-text

   ;; Conditions
   #:sophisticated-clipboard-error
   #:clipboard-unavailable
   #:clipboard-unavailable-reason
   #:not-installed
   #:not-installed-programs
   #:clipboard-command-failed
   #:clipboard-command-failed-command
   #:clipboard-command-failed-exit-code
   #:clipboard-command-failed-output
   #:clipboard-unsupported-type
   #:clipboard-unsupported-type-mime-type
   #:clipboard-unsupported-type-backend

   ;; Utilities
   #:executable-find))
