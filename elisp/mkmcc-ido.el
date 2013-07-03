;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customizing ido mode
;;
(require 'dash)

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
  "sensible defaults for ido."
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


;; enable recent files mode.
(recentf-mode t)

(setq recentf-save-file (expand-file-name "recentf" savefile-dir)
      recentf-max-saved-items 50
      recentf-max-menu-items 15)

;; completion in the minibuffer
(icomplete-mode t)                       ; completion in minibuffer
(setq icomplete-prospects-height 2)      ; don't spam my minibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-ido)
