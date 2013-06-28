;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keybindings
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings expect interactive commands.  define a shorthand for
;; interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

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
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general editing and navigation
(global-set-key (kbd "<delete>") 'delete-char)   ; delete == delete
(global-set-key (kbd "M-g")      'goto-line)     ; M-g  'goto-line
; revert buffer with no fuss
(global-set-key (kbd "M-ESC") (λ revert-buffer t t))

(global-set-key [(control shift up)]     'prelude-move-line-up)
(global-set-key [(control shift down)]   'prelude-move-line-down)
(global-set-key [(shift return)]         'prelude-smart-open-line)
(global-set-key [(control shift return)] 'prelude-smart-open-line-above)
(global-set-key (kbd "M-o")              'prelude-smart-open-line)
(global-set-key (kbd "M-O")              'prelude-smart-open-line-above)

(global-set-key [f2] 'ispell-word)
(global-set-key [f12] (λ (shell-command "open -a /Applications/Dictionary.app")))

(global-set-key (kbd "C-^") 'prelude-top-join-line)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-Z") nil)
(global-set-key (kbd "C-x C-Z") nil)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "M-/") 'hippie-expand)

;; duplicate the current line or region
(global-set-key (kbd "C-c d") 'prelude-duplicate-current-line-or-region)

;; File finding
(global-set-key (kbd "M-`")   'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; swap windows
(global-set-key (kbd "C-c s") 'prelude-swap-windows)

;; kill other buffers
(global-set-key (kbd "C-c k o") 'prelude-kill-other-buffers)

;; open in external application
(global-set-key (kbd "C-c o") 'prelude-open-with)


(global-set-key (kbd "C-c D") 'prelude-delete-file-and-buffer)
(global-set-key (kbd "C-c r") 'prelude-rename-file-and-buffer)

;; use regex versions of search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Activate occur easily inside isearch (this is great!)
(define-key isearch-mode-map (kbd "C-o")
  (λ
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programming

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'prelude-cleanup-buffer)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "M-i") 'prelude-ido-goto-symbol)

;; Indentation help
(global-set-key (kbd "C-M-\\") 'prelude-indent-region-or-buffer)
(global-set-key (kbd "C-M-z")  'prelude-indent-defun)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'prelude-eval-and-replace)

(global-set-key [(control tab)] 'tag-complete-symbol) ; tags
(global-set-key (kbd "C-x C-t") 'mkmcc-update-tags)

(global-set-key [f7] 'compile)          ; can't live without it!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)
;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (λ  (eshell t)))
;; open an ansi-term buffer
(global-set-key (kbd "C-x t") 'prelude-visit-term-buffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; url

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x C-h") 'prelude-view-url)

;; search with google
(global-set-key (kbd "C-c g") 'prelude-google)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-global-keybindings)
