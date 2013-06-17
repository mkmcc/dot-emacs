;;; Code:

;; Rake files are ruby, too, as are gemspecs, rackup files, and gemfiles.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

;; yari provides a nice Emacs interface to ri
(require 'yari)
(require 'ruby-block)
(require 'ruby-end)

(diminish 'ruby-block-mode)
(diminish 'ruby-end-mode)

(eval-after-load 'ruby-mode
  '(progn
     (ruby-block-mode t)
     (setq ruby-block-highlight-toggle 'overlay)))

(defun prelude-ruby-mode-hook ()
  (inf-ruby-setup-keybindings)
  ;; turn off the annoying input echo in irb
  (setq comint-process-echoes t)
  (local-set-key (kbd "C-h r") 'yari))

(define-key 'help-command (kbd "R") 'yari)

(add-hook 'ruby-mode-hook 'prelude-ruby-mode-hook)

(defun prelude-css-mode-hook ()
  (setq css-indent-offset 2)
  (rainbow-mode +1))

(add-hook 'css-mode-hook 'prelude-css-mode-hook)

(provide 'prelude-ruby)

;;; prelude-ruby.el ends here
