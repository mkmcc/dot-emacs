;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; latex mode tweaks
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; locate non-ascii characters
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mkmcc-set-latex-compile-command ()
  "create a buffer-local compile command for latex buffers."
  (when buffer-file-name
    (let ((cmd "rubber -df --warn all"))
      (setq-local compile-command
                  (concat cmd " " buffer-file-name)))))

(defun mkmcc-view-latex-file ()
  "find the pdf associated with a latex file and open it."
  (interactive)
  (when buffer-file-name
    (shell-command
     (concat "open "
             (file-name-sans-extension buffer-file-name)
             ".pdf"))))

(defun mkmcc-latex-mode-hook ()
  "defaults for `latex-mode'."
  (turn-on-reftex)
  (diminish 'reftex-mode)
  (diminish 'abbrev-mode "…")
  (turn-on-auto-fill)

  (mkmcc-set-latex-compile-command)
  (local-set-key (kbd "<f8>") 'mkmcc-view-latex-file))

(add-hook 'latex-mode-hook 'mkmcc-latex-mode-hook)
(add-hook 'LaTeX-mode-hook 'mkmcc-latex-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mkmcc-latex)
