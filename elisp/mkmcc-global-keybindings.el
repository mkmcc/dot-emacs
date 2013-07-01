;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keybindings
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings expect interactive commands.  define a shorthand for
;; interactive lambdas
(global-set-key (kbd "s-l") (λ (insert "\u03bb")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fix the keybindings on osx
(setq mac-command-modifier 'meta)
(setq mac-option-modifier  'super)
(setq ns-function-modifier 'hyper)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance
;;
(global-set-key [f1] 'toggle-fullscreen) ; fullscreen!

;; Font size
(global-set-key (kbd "C-M-=") 'text-scale-increase)
(global-set-key (kbd "C-M--") 'text-scale-decrease)
(global-unset-key (kbd "C-x C-+"))
(global-unset-key (kbd "C-x C--"))

;; opacity
(global-set-key (kbd "C-M-8") (λ (adjust-opacity nil -5)))
(global-set-key (kbd "C-M-9") (λ (adjust-opacity nil  5)))
(global-set-key (kbd "C-M-0") (λ (modify-frame-parameters nil `((alpha . 100)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general editing and navigation
;;
(global-set-key (kbd "<delete>") 'delete-char)   ; delete == delete
(global-set-key (kbd "M-g")      'goto-line)     ; M-g  'goto-line
(global-set-key (kbd "M-ESC") (λ (revert-buffer t t))) ; revert buffer

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-z")     nil)
(global-set-key (kbd "C-x C-z") nil)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-d") 'dired)

(global-set-key (kbd "M-/") 'hippie-expand)

;; expand-region and smart-forward
(global-set-key (kbd "M-=")       'er/expand-region)
(global-set-key (kbd "M-<up>")    'smart-up)
(global-set-key (kbd "M-<down>")  'smart-down)
(global-set-key (kbd "M-<left>")  'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)

;; File finding
(global-set-key (kbd "C-x C-r") 'prelude-recentf-ido-find-file)

;; buffer commands
(global-set-key (kbd "C-c s")   'prelude-swap-windows)
(global-set-key (kbd "C-c k o") 'prelude-kill-other-buffers)
(global-set-key (kbd "C-c o")   'prelude-open-with)
(global-set-key (kbd "C-c D")   'prelude-delete-file-and-buffer)
(global-set-key (kbd "C-c r")   'prelude-rename-file-and-buffer)

;; use regex versions of search
(global-set-key (kbd "C-s")   'isearch-forward-regexp)
(global-set-key (kbd "C-r")   'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spelling and dictionaries
(global-set-key [f2] 'ispell-word)
(defvar mkmcc-dictionary-command "open -a /Applications/Dictionary.app")
(global-set-key [f12] (λ (shell-command mkmcc-dictionary-command)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; manipulate lines
;;
(global-set-key (kbd "C-S-<up>")   'prelude-move-line-up)
(global-set-key (kbd "C-S-<down>") 'prelude-move-line-down)
(global-set-key (kbd "S-RET")      'prelude-smart-open-line)
(global-set-key (kbd "C-S-RET")    'prelude-smart-open-line-above)
(global-set-key (kbd "M-o")        'prelude-smart-open-line)
(global-set-key (kbd "M-O")        'prelude-smart-open-line-above)

(global-set-key [remap kill-whole-line] 'prelude-kill-whole-line)

;; duplicate the current line or region
(global-set-key (kbd "C-c d") 'prelude-duplicate-current-line-or-region)

;; join lines from above and below
(global-set-key (kbd "C-^") 'prelude-top-join-line)
(global-set-key (kbd "M-^") 'delete-indentation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; whitespace

;; delete whitespace around point
(global-set-key (kbd "M-SPC") 'just-one-space)
(global-set-key (kbd "M-\\")  'just-one-space)

(global-set-key (kbd "C-x \\") 'align-regexp)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'prelude-cleanup-buffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programming

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "M-i") 'prelude-ido-goto-symbol)

;; Indentation help
(global-set-key (kbd "C-M-\\") 'prelude-indent-region-or-buffer)
(global-set-key (kbd "C-M-z")  'prelude-indent-defun)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'prelude-eval-and-replace)

(global-set-key (kbd "C-TAB")   'tag-complete-symbol) ; tags
(global-set-key (kbd "C-x C-t") 'mkmcc-update-tags)

(global-set-key [f7] 'compile)          ; can't live without it!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shells
(global-set-key (kbd "C-c m") 'eshell)
(global-set-key (kbd "C-c t") 'prelude-visit-term-buffer)
(global-set-key (kbd "C-c i") 'prelude-visit-ielm-buffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; url
;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x C-h") 'prelude-view-url)

;; search with google
(global-set-key (kbd "C-c g") 'prelude-google)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; key chords
;; (require 'key-chord)

;; (key-chord-define-global "jj" 'ace-jump-word-mode)
;; (key-chord-define-global "jl" 'ace-jump-line-mode)
;; (key-chord-define-global "jk" 'ace-jump-char-mode)
;; (key-chord-define-global "JJ" 'prelude-switch-to-previous-buffer)
;; (key-chord-define-global "uu" 'undo-tree-visualize)

;; (key-chord-mode +1)


(provide 'mkmcc-global-keybindings)
