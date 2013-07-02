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

(defvar reftex-docstruct-symbol)
(defun mkmccc-find-reftex-label ()
  "Prompt for a label (with completion) and return it."
  (interactive)
  (reftex-access-scan-info)
  (let* ((docstruct (symbol-value reftex-docstruct-symbol))
	 (label (completing-read "Label: " docstruct
                                 (lambda (x) (stringp (car x))) t nil nil)))
    label))

(defun mkmcc-latex-mode-hook ()
  "defaults for `latex-mode'."
  (turn-on-reftex)
  (diminish 'reftex-mode)
  (diminish 'abbrev-mode "…")
  (turn-on-auto-fill)

  ;; F7 to compile, F8 to view the file; keep F12 for dictionary.
  (mkmcc-set-latex-compile-command)
  (local-unset-key (kbd "<f12>"))
  (local-set-key (kbd "<f8>") 'mkmcc-view-latex-file))

(add-hook 'latex-mode-hook 'mkmcc-latex-mode-hook)
(add-hook 'LaTeX-mode-hook 'mkmcc-latex-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mkmcc-latex)
