;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions used in later init files.
;;
(require 'cl)                           ; can't use dash yet!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; system type: useful to have
(defconst mkmcc-linux-p
  (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defconst mkmcc-macosx-p
  (eq system-type 'darwin))
(defconst mkmcc-console-p
  (eq (symbol-value 'window-system) nil))
(defconst mkmcc-machine
  (substring (shell-command-to-string "hostname") 0 -1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define shortcuts for frequently used things
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load path utilities
(defun mkmcc-add-subfolders-to-load-path (parent-dir &optional the-list)
  "Adds all first level `parent-dir' subdirs to a list.  Default
to the Emacs load path."
  (let ((mlist (if the-list the-list 'load-path )))
    (mkmcc-add-subfolders-to-list parent-dir mlist)))

(defun mkmcc-add-subfolders-to-list (parent-dir the-list)
  "Adds all first level `parent-dir' subdirs to a list."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list the-list name)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-core)
