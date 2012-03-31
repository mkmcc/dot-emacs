;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keybindings
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fix the keybindings on osx
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(global-set-key [f2] 'ispell-word)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance
;;
(global-set-key [f1] 'ns-toggle-fullscreen) ; fullscreen!

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general editing and navigation
(global-set-key (kbd "<delete>") 'delete-char)   ; delete == delete
(global-set-key (kbd "M-g")      'goto-line)     ; M-g  'goto-line
(global-set-key (kbd "C-M-g")    'revert-buffer) ;

(global-set-key [(control shift up)]   'prelude-move-line-up)
(global-set-key [(control shift down)] 'prelude-move-line-down)

(global-set-key (kbd "C-Z") nil)
(global-set-key (kbd "C-x C-Z") nil)

(global-set-key (kbd "C-x C-b") nil)    ; turn off list-buffers (annoying)

(global-set-key (kbd "M-/") 'hippie-expand)

;; duplicate the current line or region
(global-set-key (kbd "C-c d") 'prelude-duplicate-current-line-or-region)

;; File finding
(global-set-key (kbd "C-x f") 'prelude-recentf-ido-find-file)
(global-set-key (kbd "C-c r") 'bury-buffer)
(global-set-key (kbd "M-`")   'file-cache-minibuffer-complete)

;; swap windows
(global-set-key (kbd "C-c s") 'prelude-swap-windows)

;; kill other buffers
(global-set-key (kbd "C-c k o") 'prelude-kill-other-buffers)

;; open in external application
(global-set-key (kbd "C-c o") 'prelude-open-with)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programming

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'prelude-cleanup-buffer)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "M-i") 'prelude-ido-goto-symbol)

;; Indentation help
(global-set-key (kbd "C-M-\\") 'prelude-indent-region-or-buffer)

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
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))
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


;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))


(provide 'mkmcc-global-keybindings)
