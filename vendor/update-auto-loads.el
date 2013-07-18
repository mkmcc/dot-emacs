;; update-auto-loads.el --- process autoload cookies in the vendor dir
;;
;; Autoloading makes function names available to emacs, without having
;; to load the entire file which defines them.  When you call the
;; function, emacs loads the file to retrieve the function definition.
;; So rather than loading everything at startup, you can load things
;; piecemeal as you use them.  neat!
;;
;; An autoload statement has the form (autoload 'smex "smex"), where
;; the first argument is the function name and the second is the name
;; of the file which defines it (without the .el extension; including
;; the .el extension will cause it to ignore byte-compiled files!).
;;
;; Compared to `require', autoload statements are a pain to maintain
;; manually -- you need to list every function you might want.
;; Fortunately, emacs can create them automatically.  You tell emacs
;; which functions to autoload by prefacing their definitions with the
;; 'magic' comment ;;;###autoload.  Emacs generates the autoload
;; statements and saves them to a file called `loaddefs.el'.  You can
;; then load that in your init file.
;;
;; If autoload doesn't recognize the statement as a defun, it simply
;; copies the whole expression into loaddefs.el.  You can use this for
;; quick things like adding to auto-mode-alist, etc.
;;
;; Emacs already processes autoload statements for built-in things and
;; for things installed via package.el.  This file adds functionality
;; for things you installed manually.  Specifically, it assumes you
;; have put code in the folder stored in `vendor-dir', and it
;; processes every .el file in that dir and in the first level of
;; subdirectories.  The output is a loaddefs.el inside vendor-dir,
;; which you can load explicitly at the end of your init.
;;
(require 'autoload)
(require 'dash)
(defvar vendor-dir)

(defun mkmcc/file-real-directory-p (file-full)
  "Return true if FILE-FULL is *really* a directory. (i.e., not
'.' or '..').  FILE-FULL should be a full path, so that
`file-directory-p' recognizes it."
  (let ((file-small (file-name-nondirectory file-full)))
    (and (file-directory-p file-full)
         (not (equal file-small ".."))
         (not (equal file-small ".")))))

;;;###autoload
(defun update-vendor-autoloads (dir)
  "Process autoloads in DIR and first-level subdirectories."
  (interactive)
  (let ((generated-autoload-file (expand-file-name "loaddefs.el" dir)))
    (when (not (file-exists-p generated-autoload-file))
      ;; create the file with non-zero size to appease autoload
      (with-current-buffer (find-file-noselect generated-autoload-file)
        (insert ";;")
        (save-buffer)))
    (-map 'update-directory-autoloads
          (-insert-at 0 dir
                      (-filter 'mkmcc/file-real-directory-p
                               (directory-files dir t))))))

;;;###autoload
(add-hook 'kill-emacs-hook
          (lambda ()
            (message "Updating vendor autoloads...")
            (update-vendor-autoloads vendor-dir)
            (message "Updating vendor autoloads... done.")))

;;; update-auto-loads.el ends here
