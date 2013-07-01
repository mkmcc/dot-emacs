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

(defun prelude-visit-ielm-buffer ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (prelude-start-or-switch-to 'ielm "*ielm*"))

(defun mkmcc-emacs-lisp-mode-hook ()
  "defaults for `emacs-lisp-mode'."
  (prelude-lisp-coding-hook)
  (turn-on-eldoc-mode)
  (mkmcc-remove-elc-on-save)
  ; doc checker is super annoying
  (setq-local flycheck-checkers '(emacs-lisp)))

(add-hook 'emacs-lisp-mode-hook 'mkmcc-emacs-lisp-mode-hook)


;; ielm is an interactive Emacs Lisp shell
(defun mkmcc-ielm-mode-hook ()
  "defaults for `ielm'."
  (prelude-interactive-lisp-coding-hook)
  (turn-on-eldoc-mode))

; (add-hook 'ielm-mode-hook 'mkmcc-ielm-mode-hook)
(add-hook 'ielm-mode-hook (lambda ()
                            (run-hooks 'mkmcc-ielm-mode-hook)))

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(after-load 'elisp-slime-nav
  (diminish 'elisp-slime-nav-mode))
(after-load 'rainbow
  (diminish 'rainbow-mode))
(after-load 'eldoc
  (diminish 'eldoc-mode))

;; enable elisp-slime-nav-mode
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-emacs-lisp)
