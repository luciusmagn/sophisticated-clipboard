(in-package #:sophisticated-clipboard)

;;;; -- Backend Detection --

(defun host-operating-system ()
  "Return :WINDOWS, :DARWIN, or :POSIX for the running host."
  (cond
    ((uiop:os-windows-p) :windows)
    ((uiop:os-macosx-p) :darwin)
    (t :posix)))

(defun environment-set-p (value)
  "Return true when environment VALUE is a non-empty string."
  (and (stringp value) (plusp (length value))))

(defun clipboard-detect-backend
    (&key (getenv #'uiop:getenv)
          (executable-find #'executable-find)
          (operating-system (host-operating-system)))
  "Return the backend reaching this process's clipboard.

Windows uses the Win32 API. macOS uses pbcopy and pbpaste. Other systems,
including the BSDs, prefer wl-clipboard under WAYLAND_DISPLAY and otherwise
xclip or xsel under DISPLAY; a Wayland session lacking wl-clipboard falls
back to its X11 tools when DISPLAY is also set, as it is under XWayland.
GETENV and EXECUTABLE-FIND are replaceable for tests. Signals
CLIPBOARD-UNAVAILABLE, or its subtype NOT-INSTALLED, when nothing applies."
  (flet ((found-p (command)
           (not (null (funcall executable-find command)))))
    (ecase operating-system
      (:windows
       #+os-windows (make-instance 'windows-backend)
       #-os-windows (error 'clipboard-unavailable
                           :reason "this image was not built for Windows"))
      (:darwin
       (if (and (found-p "pbcopy") (found-p "pbpaste"))
           (make-instance 'darwin-backend)
           (error 'not-installed :programs '("pbcopy" "pbpaste"))))
      (:posix
       (let ((wayland-p (environment-set-p (funcall getenv "WAYLAND_DISPLAY")))
             (x11-p (environment-set-p (funcall getenv "DISPLAY"))))
         (cond
           ((and wayland-p (found-p "wl-copy") (found-p "wl-paste"))
            (make-instance 'wayland-backend))
           ((and x11-p (found-p "xclip"))
            (make-instance 'x11-backend :tool :xclip))
           ((and x11-p (found-p "xsel"))
            (make-instance 'x11-backend :tool :xsel))
           ((or wayland-p x11-p)
            (error 'not-installed
                   :programs (append (and wayland-p '("wl-copy" "wl-paste"))
                                     (and x11-p '("xclip" "xsel")))))
           (t
            (error 'clipboard-unavailable
                   :reason "neither WAYLAND_DISPLAY nor DISPLAY is set"))))))))

(defun clipboard-backend ()
  "Return *CLIPBOARD-BACKEND* or detect the backend for this process."
  (or *clipboard-backend* (clipboard-detect-backend)))

(defun clipboard-available-p ()
  "Return true when some backend can reach a clipboard from this process."
  (handler-case
      (progn (clipboard-backend) t)
    (sophisticated-clipboard-error ()
      nil)))
