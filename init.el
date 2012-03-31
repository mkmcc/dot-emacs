;;; Code:
(require 'cl)

(defvar base-dir     "~/.emacs.d/")
(defvar elisp-dir    (concat base-dir "elisp/"))
(defvar vendor-dir   (concat base-dir "vendor/"))
(defvar themes-dir   (concat base-dir "themes/"))
(defvar personal-dir (concat base-dir "personal/")
  "All Emacs Lisp files here are loaded automatically by
Prelude.")

(add-to-list 'load-path elisp-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path personal-dir)
(add-to-list 'load-path themes-dir)


;; config changes made through the customize UI will be store here
(setq custom-file (concat personal-dir "custom.el"))

;; packages
(require 'mkmcc-packages)
(require 'prelude-el-get)

;; core functions
(require 'prelude-core)
(require 'mkmcc-core)

;; interface
(require 'mkmcc-ui)
(require 'mkmcc-editor)
(require 'mkmcc-autocomplete)
(require 'mkmcc-global-keybindings)

;; programming & markup languages support
(require 'prelude-programming)
(require 'mkmcc-c)
(require 'mkmcc-latex)
(require 'prelude-python)
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

;; load the personal settings (this includes `custom-file')
(when (file-exists-p personal-dir)
  (mapc 'load (directory-files personal-dir nil "^[^#].*el$")))

;;; fin
