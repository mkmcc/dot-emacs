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

;;; TODO check for window system
(require 'solarized-theme)
(load-theme 'solarized-light t)
(add-to-list 'default-frame-alist '(font . "Menlo-13"))
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
  (set-face-background 'show-paren-match-face
                       (face-background 'default))
  (set-face-foreground 'show-paren-match-face
                       "DodgerBlue")
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
