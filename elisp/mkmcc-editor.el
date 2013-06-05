;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editor behavior
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use utf-8
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'expand-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spell check
(eval-after-load "ispell"               ; why in the eval-after-load?
  (setq ispell-silently-savep t
        ispell-program-name   "aspell"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(add-hook 'message-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook    'flyspell-mode)

(eval-after-load "flyspell"
  '(diminish 'flyspell-mode))
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
      delete-old-versions t
      vc-make-backup-files t)

;; autosave on window switches
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun prelude-auto-save-command ()
  "Save the current buffer if appropriate."
  (when (and buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (save-buffer)))

(defadvice switch-to-buffer (before save-buffer-now activate)
  "Invoke `prelude-auto-save-command' before `switch-to-window'."
  (prelude-auto-save-command))
(defadvice other-window (before other-window-now activate)
  "Invoke `prelude-auto-save-command' before `other-window'."
  (prelude-auto-save-command))
(defadvice windmove-up (before other-window-now activate)
  "Invoke `prelude-auto-save-command' before `windmove-up'."
  (prelude-auto-save-command))
(defadvice windmove-down (before other-window-now activate)
  "Invoke `prelude-auto-save-command' before `windmove-down'."
  (prelude-auto-save-command))
(defadvice windmove-left (before other-window-now activate)
  "Invoke `prelude-auto-save-command' before `windmove-left'."
  (prelude-auto-save-command))
(defadvice windmove-right (before other-window-now activate)
  "Invoke `prelude-auto-save-command' before `windmove-right'."
  (prelude-auto-save-command))

(add-hook 'mouse-leave-buffer-hook 'prelude-auto-save-command)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" savefile-dir))
(setq-default save-place t)
(require 'saveplace)

;; savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" savefile-dir))
(savehist-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (progn
    (switch-to-buffer
     (get-buffer-create (generate-new-buffer-name "*scratch*")))
    (emacs-lisp-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings 'super)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; volatiile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; note - this should be after volatile-highlights is required
;; add the ability to copy and cut the current line, without marking it
(defadvice kill-ring-save (before smart-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before smart-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mkmcc-editor)
