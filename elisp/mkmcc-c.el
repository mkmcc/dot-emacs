;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-mode
;;;
;;; Time-stamp: <2012-02-02 09:02:21 (mkmcc)>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile a project
(defun mkmcc-compile-pkg (&optional command startdir)
  "Compile a package, moving up to the parent directory
containing configure.ac, if it exists. Otherwise, move to the
parent directory containing Makefile.  Start in startdir if
defined, else start in the current directory."
  (interactive)

  (let ((dirname)
        (dir-buffer nil)
        (startdir (expand-file-name (if startdir startdir ".")))
        (command (if command command compile-command)))

    (setq dirname (mkmcc-upward-find-file "configure.ac" startdir))
    (unless dirname
      (setq dirname (mkmcc-upward-find-file "Makefile" startdir)))
    (unless dirname
      (setq dirname (expand-file-name ".")))

    (save-excursion
      (setq dir-buffer (find-file-noselect dirname))
      (set-buffer dir-buffer)
      (compile command)
      (kill-buffer dir-buffer))))

(defun mkmcc-compile ()
  "Like `compile', but uses `mkmcc-compile-pkg'"
  (interactive)
  (mkmcc-compile-pkg compile-command))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-mode-common
(defun mkmcc-c-mode-common ()
  (setq
   c-basic-offset 2
   indent-tabs-mode nil
   c-hungry-delete-key t)

  ;; Keybindings
  (local-set-key (kbd "C-c o")   'ff-find-other-file)
  (local-set-key (kbd "C-c C-c") 'comment-region)
  (local-set-key (kbd "C-c C-u") 'uncomment-region)
  (local-set-key (kbd "<f8>")    'mkmcc-compile))

;; this will affect all modes derived from cc-mode, like
;; java-mode, php-mode, etc
(add-hook 'c-mode-common-hook
          'mkmcc-c-mode-common)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mkmcc-c)
