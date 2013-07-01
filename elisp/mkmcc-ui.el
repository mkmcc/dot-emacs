;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tweaks to user interface
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance (superficial)
(scroll-bar-mode 0)                     ; disable useless things
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

(blink-cursor-mode -1)                  ; annoyances
(setq inhibit-startup-screen t)

(require 'server)

(defvar mkmcc-fixed-pitch    "Menlo-13")
(defvar mkmcc-variable-pitch "Warnock Pro-13")

(defun setup-window-system-frame-colours (&rest frame)
  (unless (eq window-system nil)
    (load-theme 'solarized-light t)
    (set-frame-font mkmcc-fixed-pitch)
    (set-face-attribute 'fixed-pitch    nil :font mkmcc-fixed-pitch)
    (set-face-attribute 'variable-pitch nil :font mkmcc-variable-pitch)))

(defadvice server-create-window-system-frame
  (after set-window-system-frame-colours ())
  "Set custom frame colours when creating the first frame on a display"
  (setup-window-system-frame-colours))

(ad-activate 'server-create-window-system-frame)

(add-hook 'after-make-frame-functions 'setup-window-system-frame-colours t)

(defun adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coding -> reading
(defvar writeroom-mode-line nil
  "storage variable for the `mode-line-format' variable")

(defun writeroom-enable ()
  "set generous margins and delete the fringe."
  (interactive)
  (delete-other-windows)
  (let ((margin
         (/ (- (window-body-width) fill-column) 3)))
    (setq left-margin-width  margin
          right-margin-width 0
          left-fringe-width  0
          right-fringe-width 0)
    (setq writeroom-mode-line mode-line-format
          mode-line-format    nil))
  (set-window-buffer nil (current-buffer)))

(defun writeroom-disable ()
  "restore margins and fringe."
  (interactive)
  (setq left-margin-width  0
        right-margin-width 0
        left-fringe-width  nil
        right-fringe-width nil)
  (setq mode-line-format    writeroom-mode-line
        writeroom-mode-line nil)
  (set-window-buffer nil (current-buffer)))

(defun readability ()
  (interactive)
  ;; buffer-face-mode is both a function and a variable
  (if (and (boundp 'buffer-face-mode)
           (symbol-value 'buffer-face-mode))
      (progn
        (variable-pitch-mode -1)
        (text-scale-increase -3)
        (setq line-spacing nil)
        (writeroom-disable))
    (progn
      (variable-pitch-mode t)
      (text-scale-increase 3)
      (setq line-spacing 7)
      (writeroom-enable))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convenience
(fset 'yes-or-no-p 'y-or-n-p)           ; enable y/n answers

(put 'narrow-to-region 'disabled nil)   ; enable...
(put 'downcase-region 'disabled nil)    ; ...
(put 'upcase-region 'disabled nil)      ; ...
(put 'erase-buffer 'disabled nil)       ; ...useful things

(defvar cua-enable-cua-keys)
(defvar cua-delete-selection)
(setq cua-enable-cua-keys  nil          ; only for rectangles
      cua-delete-selection nil)         ; don't delete selection
(cua-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance (behavior)
(show-paren-mode t)                     ; defaults are ugly
(defvar show-paren-style)
(setq show-paren-style 'parenthesis)
(set-face-attribute 'show-paren-match-face nil
                    :weight 'extra-bold :underline nil
                    :overline nil       :slant 'normal)

;; smart indenting and pairing for all
(electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)
(setq require-final-newline t)          ; end files with a newline

(setq ring-bell-function 'ignore)       ; turn off the damn bell

(setq kill-buffer-query-functions       ; don't prompt me
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(setq redisplay-dont-pause t)           ; more responsive display
(setq-default indent-tabs-mode nil)     ; death to tabs!

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(set-default 'imenu-auto-rescan t)

;; revert buffers automatically when underlying files are changed
;; externally
(global-auto-revert-mode t)

;; Also auto refresh dired, but be quiet about it
(defvar global-auto-revert-non-file-buffers)
(defvar auto-revert-verbose)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Use elisp ls program.  The osx one doesn't have the full GNU
;; functionality.
(require 'ls-lisp)
(setq ls-lisp-ignore-case t)            ; ignore case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; games
(defvar tetris-score-file)
(setq tetris-score-file                 ; keep my ~/ clean
      "~/.emacs.d/games/tetris-scores")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation window
(setq compilation-scroll-output 'first-error ; scroll until first error
      compilation-read-command t             ; require enter to compile
      compilation-window-height 16)          ; keep it readable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fullscreen
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-ui)
