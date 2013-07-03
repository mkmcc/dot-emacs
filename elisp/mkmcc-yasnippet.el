;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load yasnippet
;;
(defvar snippets-dir)
(defvar yas-snippet-dirs)
(defvar yas-prompt-functions)
(defvar yas-verbosity)

(autoload 'yas-minor-mode "yasnippet.el")

(after-load 'yasnippet
  (setq yas-snippet-dirs snippets-dir)
  (yas-load-directory yas-snippet-dirs)
  (diminish 'yas-minor-mode))

(defun mkmcc-enable-yasnippet ()
  (yas-minor-mode +1))

(defun mkmcc-disable-yasnippet ()
  (yas-minor-mode -1))

;; only use yasnippet for c, latex, and ruby modes.
(add-hook 'ruby-mode-hook  'mkmcc-enable-yasnippet)
(add-hook 'latex-mode-hook 'mkmcc-enable-yasnippet)
(add-hook 'LaTeX-mode-hook 'mkmcc-enable-yasnippet)
(add-hook 'c-mode-hook     'mkmcc-enable-yasnippet)

;; term-mode does not play well with yasnippet
(add-hook 'term-mode-hook 'mkmcc-disable-yasnippet)

;; No dropdowns please, yas
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

;; No need to be so verbose
(setq yas-verbosity 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-yasnippet)
