;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editor behavior
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; character stuff
;; UTF-8 please
(setq locale-coding-system   'utf-8)    ; pretty
(set-terminal-coding-system  'utf-8)    ; pretty
(set-keyboard-coding-system  'utf-8)    ; pretty
(set-selection-coding-system 'utf-8)    ; please
(prefer-coding-system        'utf-8)    ; with sugar on top

(setq-default indent-tabs-mode nil)  ; don't use tabs to indent
(setq-default tab-width 8)           ; but maintain correct appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expand-region with delete-selection-mode is handy
(autoload 'expand-region "expand-region.el")
(after-load 'expand-region
  (if (boundp 'cua-delete-selection)
      (setq cua-delete-selection t)
    (delete-selection-mode t)))

(autoload 'smart-up       "smart-forward.el")
(autoload 'smart-down     "smart-forward.el")
(autoload 'smart-forward  "smart-forward.el")
(autoload 'smart-backward "smart-forward.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spell check
(autoload 'flyspell-mode "flyspell.el")

(add-hook 'message-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook    'flyspell-mode)

(defvar ispell-silently-savep t)
(defvar ispell-program-name "aspell")
(after-load 'flyspell (diminish 'flyspell-mode))
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
(defvar time-stamp-active t)
(defvar time-stamp-line-limit 10)
(defvar time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)")
(add-hook 'write-file-hooks 'time-stamp)

;; magit -- run `magit-status' in full-screen mode.  save the previous
;; window configuration and return to it afterwards
(after-load 'magit
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp mode
;;
(defvar tramp-default-method "ssh")
(defvar tramp-default-user "mkmcc")
(defvar tramp-default-host "strata")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(defvar save-place-file (expand-file-name "saveplace" savefile-dir))
(setq-default save-place t)

;; savehist keeps track of some history
(defvar savehist-additional-variables '(search ring regexp-search-ring))
(defvar savehist-autosave-interval 60)
(defvar savehist-file (expand-file-name "savehist" savefile-dir))
(savehist-mode +1)
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


;; shorter aliases for ack-and-a-half commands
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)



;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode yank-indent-blacklisted-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode yank-indent-blacklisted-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-auto-mode 'shell-script-mode "\\.zsh\\'")

;; saner regex syntax (maybe switch to rx?)
;;   NB: C-c C-w copies and converts to elisp format
;;   NB 2: C-c TAB cycles formats
(defvar reb-re-syntax 'string)

;; whitespace-mode config
(defvar whitespace-line-column 80)
(defvar whitespace-style
  '(face space tabs newline empty trailing lines-tail
         space-mark tabs-mark newline-mark))
(after-load 'whitespace
  (diminish 'whitespace-mode))

(defun prelude-enable-whitespace ()
  "enable `whitespace-mode'."
  (add-hook 'before-save-hook 'whitespace-cleanup nil t)
  (whitespace-mode +1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-editor)
