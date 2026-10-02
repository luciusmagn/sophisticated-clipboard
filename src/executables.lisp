(in-package #:sophisticated-clipboard)

;;;; -- Locating Helper Programs --

(defun path-directories (path)
  "Return the directories listed in the PATH-style string PATH, in search order."
  (when path
    (loop for entry in (uiop:split-string
                        path
                        :separator (string (if (uiop:os-windows-p) #\; #\:)))
          unless (string= entry "")
            collect (uiop:ensure-directory-pathname entry))))

(defun executable-extensions ()
  "Return the file extensions a bare command name may carry on this host."
  (if (uiop:os-windows-p)
      (let ((pathext (uiop:getenv "PATHEXT")))
        (or (and pathext
                 (remove "" (uiop:split-string pathext :separator ";")
                         :test #'string=))
            '(".EXE" ".CMD" ".BAT")))
      '("")))

(defun executable-find (command &key (path (uiop:getenv "PATH")))
  "Return the first file named COMMAND in PATH's directories, or NIL.

PATH defaults to the process environment. On Windows the PATHEXT extensions
are tried in order, so a bare name finds its .exe or .cmd."
  (loop for directory in (path-directories path)
        do (loop for extension in (executable-extensions)
                 for candidate = (uiop:file-exists-p
                                  (uiop:merge-pathnames*
                                   (uiop:parse-native-namestring
                                    (concatenate 'string command extension))
                                   directory))
                 when candidate
                   do (return-from executable-find candidate)))
  nil)
