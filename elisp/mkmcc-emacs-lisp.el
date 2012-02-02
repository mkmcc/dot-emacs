;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs lisp

(require 'prelude-lisp)

(defun mkmcc-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun mkmcc-emacs-lisp-mode-hook ()
  (prelude-lisp-coding-hook)
  (turn-on-eldoc-mode)
  (mkmcc-remove-elc-on-save))         ; mkmcc removed rainbow mode

(add-hook 'emacs-lisp-mode-hook 'mkmcc-emacs-lisp-mode-hook)

;; ielm is an interactive Emacs Lisp shell
(defun mkmcc-ielm-mode-hook ()
  (prelude-interactive-lisp-coding-hook)
  (turn-on-eldoc-mode))

(add-hook 'ielm-mode-hook 'mkmcc-ielm-mode-hook)

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-emacs-lisp)
