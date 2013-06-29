(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customizing ido mode
;;
(ido-mode 'both)
(ido-everywhere 1)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode)

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
  ido-save-directory-list-file (expand-file-name "ido.last" savefile-dir)
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

(defun mkmcc-ido-setup ()
  "sensible "
  ;; Go straight home
  (define-key ido-file-completion-map
    (kbd "~")
    (lambda ()
      (interactive)
      (if (looking-back "/")
          (insert "~/")
        (call-interactively 'self-insert-command)))))

(add-hook 'ido-setup-hook 'mkmcc-ido-setup)

(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" savefile-dir))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf using ido
;;
;; enable recent files mode.
(recentf-mode t)

(setq recentf-save-file (expand-file-name "recentf" savefile-dir)
      recentf-max-saved-items 50
      recentf-max-menu-items 15)

(defun ido-recentf-open ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: "
                                   (-map 'abbreviate-file-name recentf-list)
                                   nil t)))
    (when file
      (find-file file))))
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


;; load yasnippet
(autoload 'yas-minor-mode "yasnippet.el")

(eval-after-load "yasnippet"
  '(progn
     (setq yas/root-directory snippets-dir)
     (yas/load-directory yas/root-directory)
     (diminish 'yas-minor-mode)))

(defun mkmcc-enable-yasnippet ()
  (yas-minor-mode +1))

(defun mkmcc-disable-yasnippet ()
  (yas-minor-mode -1))

(add-hook 'ruby-mode-hook  'mkmcc-enable-yasnippet)
(add-hook 'latex-mode-hook 'mkmcc-enable-yasnippet)
(add-hook 'LaTeX-mode-hook 'mkmcc-enable-yasnippet)
(add-hook 'c-mode-hook     'mkmcc-enable-yasnippet)

;; term-mode does not play well with yasnippet
(add-hook 'term-mode-hook 'mkmcc-disable-yasnippet)


(provide 'mkmcc-autocomplete)
