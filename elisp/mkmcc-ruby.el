;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby configuration
;;
(require 'prelude-programming)

(defvar ruby-block-highlight-toggle 'overlay)

(autoload 'yari "yari.el")              ;TODO: are these necessary?
(autoload 'ruby-block-mode "ruby-block.el")
(autoload 'ruby-end-mode "ruby-end.el")

(after-load 'ruby-block (diminish 'ruby-block-mode))
(after-load 'ruby-end   (diminish 'ruby-end-mode))

(define-key 'help-command (kbd "R") 'yari)

;; Rake files are ruby, too, as are gemspecs, rackup files, and gemfiles.
(add-auto-mode 'ruby-mode
               "\\.rake\\'"  "Rakefile\\'"    "\\.gemspec\\'"  "\\.ru\\'"
               "Gemfile\\'"  "Guardfile\\'"   "Capfile\\'"     "\\.thor\\'"
               "Thorfile\\'" "Vagrantfile\\'" "\\.jbuilder\\'")

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(defun prelude-ruby-mode-hook ()
  (inf-ruby-setup-keybindings)
  ;; turn off the annoying input echo in irb
  (setq comint-process-echoes t)
  (ruby-end-mode +1)
  (ruby-block-mode t)
  ;; CamelCase aware editing operations
  (subword-mode +1))

(add-hook 'ruby-mode-hook 'prelude-ruby-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-ruby)
