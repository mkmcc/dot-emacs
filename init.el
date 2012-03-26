;;; Code:
(require 'cl)

;; (if (eq system-type 'darwin)
;;     (push "/usr/local/bin" exec-path))   ;; need to do something better

(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-elisp-dir (concat prelude-dir "elisp/")
  "This directory houses all of the built-in Prelude module. You
should avoid modifying the configuration there.")
(defvar prelude-vendor-dir (concat prelude-dir "vendor/")
  "This directory house Emacs Lisp packages that are not yet
available in ELPA (or Marmalade).")
(defvar prelude-personal-dir (concat prelude-dir "personal/")
  "Users of Emacs Prelude are encouraged to keep their personal
configuration changes in this directory. All Emacs Lisp files
there are loaded automatically by Prelude.")

(add-to-list 'load-path prelude-elisp-dir)
(add-to-list 'load-path prelude-vendor-dir)
(add-to-list 'load-path prelude-personal-dir)

;; config changes made through the customize UI will be store here
(setq custom-file (concat prelude-personal-dir "custom.el"))

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
(when (file-exists-p prelude-personal-dir)
  (mapc 'load (directory-files prelude-personal-dir nil "^[^#].*el$")))

;;; fin
