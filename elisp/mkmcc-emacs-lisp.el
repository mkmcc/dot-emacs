;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs lisp

(require 'prelude-lisp)

(defun mkmcc-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(defun mkmcc-emacs-lisp-mode-hook ()
  "defaults for `emacs-lisp-mode'."
  (prelude-lisp-coding-hook)
  (turn-on-eldoc-mode)
  (mkmcc-remove-elc-on-save)
  (setq-local flycheck-checkers '(emacs-lisp)))

(add-hook 'emacs-lisp-mode-hook 'mkmcc-emacs-lisp-mode-hook)

;; ielm is an interactive Emacs Lisp shell
(defun mkmcc-ielm-mode-hook ()
  (prelude-interactive-lisp-coding-hook)
  (turn-on-eldoc-mode))

(add-hook 'ielm-mode-hook 'mkmcc-ielm-mode-hook)

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(eval-after-load "elisp-slime-nav"
  '(diminish 'elisp-slime-nav-mode))
(eval-after-load "rainbow-mode"
  '(diminish 'rainbow-mode))
(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-emacs-lisp)
