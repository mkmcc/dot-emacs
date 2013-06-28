;;; Code:
(require 'cl)
; (defvar *emacs-load-start* (current-time))

(defvar base-dir     "~/.emacs.d/")
(defvar elisp-dir    (expand-file-name "elisp"    base-dir))
(defvar vendor-dir   (expand-file-name "vendor"   base-dir))
(defvar themes-dir   (expand-file-name "themes"   base-dir))
(defvar snippets-dir (expand-file-name "snippets" base-dir))
(defvar savefile-dir (expand-file-name "savefile" base-dir))
(defvar personal-dir (expand-file-name "personal" base-dir)
  "All Emacs Lisp files here are loaded automatically.")

(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

(defun prelude-add-subfolders-to-load-path (parent-dir &optional the-list)
  "Adds all first level `parent-dir' subdirs to a list.  Default
to the Emacs load path."
  (let ((mlist (if the-list the-list 'load-path )))
    (prelude-add-subfolders-to-list parent-dir mlist)))

(defun prelude-add-subfolders-to-list (parent-dir the-list)
  "Adds all first level `parent-dir' subdirs to a list."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list the-list name)))))

(add-to-list 'load-path elisp-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path personal-dir)
(add-to-list 'load-path (expand-file-name "solarized" themes-dir))

(setq custom-file (expand-file-name "custom.el" personal-dir))
(setq custom-theme-directory themes-dir)

(require 'init-benchmarking)


;; packages
(require 'mkmcc-packages)               ; should come first!

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
;(require 'prelude-clojure)
(require 'prelude-common-lisp)
(require 'mkmcc-mathematica)
(require 'mkmcc-scheme)

;; productivity
(require 'mkmcc-deft)
(require 'mkmcc-ediff)
(require 'mkmcc-org)
(require 'mkmcc-gdb)

;; other
;(require 'mkmcc-erc)
;(require 'mkmcc-sudoku)
(require 'mkmcc-web)
;(require 'mkmcc-mu4e)
;(require 'mkmcc-weather)
(require 'mkmcc-shell)

;; load the personal settings (this includes `custom-file')
(when (file-exists-p personal-dir)
  (mapc 'load (directory-files personal-dir nil "^[^#].*el$")))

;; necessary to run gnuplot.  may not be the right solution?
(setenv "DISPLAY" ":0")

;; (message "My .emacs loaded in %ds"
;;          (destructuring-bind (hi lo ms) (current-time)
;;            (- (+ hi lo)
;;               (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

(message "init completed in %.2fms"
         (sanityinc/time-subtract-millis (current-time) before-init-time))

(require 'dash)
(require 's)

(defun benchmark/format-item (item)
  (concat
   (s-pad-right 20 " " (s-truncate 20 (symbol-name (car item))))
   (format "%f"   (cdr item))))

(message
 (concat
  "leading offenders:\n"
  (mapconcat 'benchmark/format-item
             (-take 10 (--sort (> (cdr it) (cdr other)) sanityinc/require-times))
             "\n")))

;;; fin
