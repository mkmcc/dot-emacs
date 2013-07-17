(require 'autoload)         ;ironic, i know
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
  "Update autoloads for files in the diretory containing this file."
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
