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

(line-number-mode t)                    ; modeline settings
(column-number-mode t)
(size-indication-mode t)

(file-name-shadow-mode 1)

(setq display-time-format "%b %e %l:%M%#p"
      display-time-load-average-threshold 1.0)
(display-time-mode 1)
(unless (string= system-name "strada.berkeley.edu")
  (setq battery-mode-line-format " [%b%p%%]")
  (display-battery-mode 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modeline
(setq-default
 mode-line-format
 '(;; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   "  "
   ;; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t (propertize "    " 'face 'mode-line-blank-face))))
   "  "
   ;; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ;; mode indicators: vc, recursive edit, major mode, etc.
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   ;; pad to right and show the time/battery status
   (:eval (propertize " " 'display '((space :align-to (- right-fringe 20)))))
   (global-mode-string global-mode-string)))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-blank-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
                    :box `(:line-width 6
                                       :color ,(face-attribute 'mode-line :background)
                                       :style nil))

(set-face-attribute 'mode-line-inactive nil
                    :box `(:line-width 6
                                       :color ,(face-attribute 'mode-line-inactive :background)
                                       :style nil))

(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#4271ae"
                    :family "Menlo"     ; should be mono-spaced
                    :box '(:line-width 2 :color "#4271ae"))

(set-face-attribute 'mode-line-blank-face nil
                    :inherit 'mode-line-face
                    :family "Menlo"     ; should be mono-spaced
                    :box `(:line-width 2
                                       :color ,(face-attribute 'mode-line :background)
                                       :style nil))

(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "#c82829"
                    :background "#ffffff"
                    :family "Menlo"     ; should be mono-spaced
                    :box '(:line-width 2 :color "#c82829"))

(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-face)

(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    :weight 'bold)

(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face
                    :family "Menlo")

(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face)

(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face)

(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face)

(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'font-lock-warning-face)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance (superficial)
;;; TODO check for window system
(load-theme 'birds-of-paradise t)
(add-to-list 'default-frame-alist '(font . "Menlo-13"))

(defun readability ()
  (interactive)

  ;; buffer-face-mode is both a function and a variable
  (if (and (boundp 'buffer-face-mode)
           (symbol-value 'buffer-face-mode))
      (progn
        (variable-pitch-mode -1)
        (setq line-spacing nil))
    (progn
      (set-face-attribute 'variable-pitch
                          nil
                          :font "Warnock Pro-22")
      (variable-pitch-mode t)
      (setq line-spacing 7))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; isearch
(setq search-highlight t                ; highlight when searching...
      query-replace-highlight t)        ; ...and replacing

(setq dired-isearch-filenames t)        ; make C-s and C-r only match filenames
                                        ; ...in dired buffers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convenience
(fset 'yes-or-no-p 'y-or-n-p)           ; enable y/n answers

(put 'narrow-to-region 'disabled nil)   ; enable...
(put 'downcase-region 'disabled nil)    ; ...
(put 'upcase-region 'disabled nil)      ; ...
(put 'erase-buffer 'disabled nil)       ; ...useful things

(setq cua-enable-cua-keys nil)          ; only for rectangles
(setq cua-delete-selection nil)         ; don't delete selection
(cua-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance (behavior)
(when (fboundp 'show-paren-mode)        ; show-paren-mode...
  (show-paren-mode t)                   ; defaults are ugly
  (setq show-paren-style 'parenthesis)
  (set-face-attribute 'show-paren-match-face nil
                      :weight 'extra-bold :underline nil
                      :overline nil       :slant 'normal))

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

;; Use elisp ls program.  The osx one doesn't have the full GNU
;; functionality.
(require 'ls-lisp)
(setq ls-lisp-ignore-case t)            ; ignore case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; games
(setq tetris-score-file                 ; keep my ~/ clean
      "~/.emacs.d/games/tetris-scores")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation window
(setq compilation-scroll-output 'first-error ; scroll until first error
      compilation-read-command t             ; require enter to compile
      compilation-window-height 16)          ; keep it readable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-ui)
