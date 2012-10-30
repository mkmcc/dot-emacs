;;; Code:
(require 'cl)

(defvar base-dir     "~/.emacs.d/")
(defvar elisp-dir    (expand-file-name "elisp/"    base-dir))
(defvar vendor-dir   (expand-file-name "vendor/"   base-dir))
(defvar themes-dir   (expand-file-name "themes/"   base-dir))
(defvar personal-dir (expand-file-name "personal/" base-dir)
  "All Emacs Lisp files here are loaded automatically.")

(add-to-list 'load-path elisp-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path personal-dir)
(add-to-list 'load-path (expand-file-name "solarized" themes-dir))

(setq custom-file (expand-file-name "custom.el" personal-dir))
(setq custom-theme-directory themes-dir)

;; packages
(require 'mkmcc-packages)

;; core functions
(require 'prelude-core)
(require 'mkmcc-core)

;; add the first level subfolders of themes and vendor
(prelude-add-subfolders-to-load-path vendor-dir)
(prelude-add-subfolders-to-load-path themes-dir 'custom-theme-load-path)

;; interface
(require 'mkmcc-ui)
(require 'mkmcc-editor)
(require 'mkmcc-autocomplete)
(require 'mkmcc-global-keybindings)
(require 'mkmcc-dired)

;; programming & markup languages support
(require 'prelude-programming)
(require 'mkmcc-c)
(require 'mkmcc-text)
(require 'mkmcc-latex)
;(require 'prelude-python)
(require 'prelude-ruby)
(require 'mkmcc-gnuplot)
(require 'mkmcc-athena)

;; lisps
(require 'mkmcc-emacs-lisp)
(require 'prelude-clojure)
(require 'prelude-common-lisp)
(require 'mkmcc-mathematica)
(require 'mkmcc-scheme)

;; productivity
(require 'mkmcc-deft)
(require 'mkmcc-ediff)
(require 'mkmcc-org)
(require 'mkmcc-gdb)

;; other
(require 'mkmcc-erc)
(require 'mkmcc-sudoku)
(require 'mkmcc-web)
(require 'mkmcc-mu4e)
(require 'mkmcc-weather)

;; load the personal settings (this includes `custom-file')
(when (file-exists-p personal-dir)
  (mapc 'load (directory-files personal-dir nil "^[^#].*el$")))

;; necessary to run gnuplot.  may not be the right solution?
(setenv "DISPLAY" ":0")

;;; fin
