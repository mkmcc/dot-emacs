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

; TODO fix ruby-end and package ruby-block for marmalade
;(require 'ruby-block)
(require 'ruby-end)

(defun prelude-ruby-mode-hook ()
  (inf-ruby-keys)
  ;; turn off the annoying input echo in irb
  (setq comint-process-echoes t)
;  (ruby-block-mode t)
  (local-set-key (kbd "C-h r") 'yari))

(add-hook 'ruby-mode-hook 'prelude-ruby-mode-hook)

(defun prelude-css-mode-hook ()
  (setq css-indent-offset 2)
  (rainbow-mode +1))

(add-hook 'css-mode-hook 'prelude-css-mode-hook)

(provide 'prelude-ruby)

;;; prelude-ruby.el ends here
