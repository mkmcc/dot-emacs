;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editor behavior
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spell check
(eval-after-load "ispell"               ; why in the eval-after-load?
  (setq ispell-silently-savep t
        ispell-program-name   "aspell"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(defun mkmcc-turn-on-flyspell ()
  "Force flyspell-mode on using a positive argument.  For use in hooks."
  (interactive)
  (flyspell-mode +1))

(add-hook 'message-mode-hook 'mkmcc-turn-on-flyspell)
(add-hook 'text-mode-hook    'mkmcc-turn-on-flyspell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; version control
;;
;; backups  (emacs will write backups and number them)
(setq make-backup-files t                                ; do make backups
      backup-by-copying t                                ; and copy them...
      backup-directory-alist '(("." . "~/.backup/"))     ; ...here
      version-control t
      kept-new-versions 5
      kept-old-versions 5
      delete-old-versions t)

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; time-stamps
(setq
  time-stamp-active t                                    ; do enable time-stamps
  time-stamp-line-limit 10                               ; first 10 buffer lines
  time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
(add-hook 'write-file-hooks 'time-stamp)                 ; update when saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp mode
;;
(setq tramp-default-method "ssh")       ; can access work computer using
                                        ; /strada.berkeley.edu:~/file
(setq tramp-default-user "mkmcc"
      tramp-default-host "strada")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; meaningful names for buffers with the same name
;; ;;
;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'post-forward ; make buffer names more unique
;;       uniquify-separator ":"
;;       uniquify-after-kill-buffer-p t
;;       uniquify-ignore-buffers-re "^\\*")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" base-dir))
(setq-default save-place t)
(require 'saveplace)

;; savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" base-dir))
(savehist-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings 'super)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mkmcc-editor)
