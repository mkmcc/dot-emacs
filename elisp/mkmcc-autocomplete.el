;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customizing ido mode
;;

(ido-mode 'both)
(ido-everywhere 1)

(setq
  ido-case-fold  t                      ; be case-insensitive
  ido-enable-flex-matching t
  ido-create-new-buffer 'prompt
  ido-max-prospects 6                   ; don't spam my minibuffer
  ido-confirm-unique-completion t)

;; searching behavior
(setq
  ido-enable-last-directory-history t   ; remember last used dirs
;  ido-use-filename-at-point nil        ; annoying
  ido-use-url-at-point nil)

;; other
(setq
  ido-save-directory-list-file "~/.emacs.d/ido.last"
  ido-ignore-buffers
  '( "\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido"
     "*compilation*" "*gnuplot errors*" ))

(setq completion-ignore-case t          ; ignore case when completing
      read-file-name-completion-ignore-case t)

;; set the file permissions on the ido.last file.  This is a potential
;; security concern.
;;; NB: recentf and saveplace already do this.
(defvar ido-save-file-modes 384
  "Mode bits for the ido save file.  Integer or nil.  If non-nil,
set the mode bits to that value.  By default give R/W access only
to the owner of the file.  See the function `set-file-modes'.")

(defadvice ido-save-history (after ido-set-file-mode)
  "Set the permissions bit for the .ido.last file"
  (when ido-save-file-modes
    (set-file-modes ido-save-directory-list-file ido-save-file-modes)))

(ad-activate 'ido-save-history)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf using ido
;;
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

(setq recentf-save-file (concat user-emacs-directory "recentf")
      recentf-max-saved-items 50
      recentf-max-menu-items 15)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hippie expand
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion in the minibuffer
(icomplete-mode t)                       ; completion in minibuffer
(setq icomplete-prospects-height 2)      ; don't spam my minibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
;; (require 'yasnippet)
;; (add-to-list 'yas/snippet-dirs
;;              (concat personal-dir "plugins/yasnippet/snippets"))

;; (yas/initialize)
;; ;(yas/reload-all)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mkmcc-autocomplete)
