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

(setq ring-bell-function 'ignore)       ; turn off the damn bell

(defvar show-paren-style 'parenthesis)
(show-paren-mode t)
(set-face-attribute 'show-paren-match-face nil
                    :weight 'extra-bold :underline nil
                    :overline nil       :slant 'normal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; frame font, colors, and other parameters
;;
;; fonts
(defvar mkmcc-fixed-pitch    "Menlo-13")
(defvar mkmcc-variable-pitch "Chaparral Pro-13")

;; don't propagate definitions from one theme to the next!
(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; don't apply fonts and colors for text-only displays
;;   NB: this won't apply changes to standalone instances of emacs.  I
;;   think this is actually a good thing: just by looking at a window
;;   I can know whether or not it is attached to the emacs daemon.
;;   Also, there is no reason to use standalone instances...
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

;; fullscreen
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
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

(defvar cua-enable-cua-keys nil)        ; only for rectangles
(defvar cua-delete-selection nil)       ; don't delete selection
(cua-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance (behavior)
;;
(setq require-final-newline t)          ; end files with a newline

;; smart indenting and pairing for all
(electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

(setq kill-buffer-query-functions       ; don't prompt me about processes
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(setq-default indent-tabs-mode nil)     ; death to tabs!

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position) 1

(setq redisplay-dont-pause t)           ; more responsive display

(set-default 'imenu-auto-rescan t)


;; revert buffers automatically when underlying files are changed
;; externally
(defvar global-auto-revert-non-file-buffers t) ; also revert dired
(defvar auto-revert-verbose nil)
(global-auto-revert-mode t)

;; Use elisp ls program.  The osx one doesn't have the full GNU
;; functionality.
(defvar ls-lisp-ignore-case t)
(autoload 'insert-directory "ls-lisp.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; games
(defvar tetris-score-file "~/.emacs.d/games/tetris-scores")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation window
(defvar compilation-scroll-output 'first-error) ; scroll until first error
(defvar compilation-read-command t)             ; require enter to compile
(defvar compilation-window-height 16)           ; keep it readable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-ui)
