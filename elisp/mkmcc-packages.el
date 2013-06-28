;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package repositories
;;

(require 'cl)
(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; repositories
;; (add-to-list 'package-archives
;;              '("gnu" . "http://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives
;;              '("ELPA" . "http://tromey.com/elpa/"))
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-user-dir (expand-file-name "elpa" base-dir))
(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatically update or install some packages
;;; NB. should not use dash list functions here -- it may not be
;;; installed yet!
(defvar mkmcc-packages
  '(melpa
    org deft
    gnuplot-mode
    rainbow-mode flycheck expand-region
    zenburn-theme volatile-highlights diminish nyan-mode
    paredit clojure-mode elisp-slime-nav ack-and-a-half
    gitconfig-mode gitignore-mode git-commit-mode magit gist
    ido-ubiquitous smex yasnippet
    ruby-end inf-ruby yari
    dash s ht loop)
  "A list of packages to ensure are installed at launch.")

(defun mkmcc-packages-installed-p ()
  "Check if all packages in `mkmcc-packages' are installed."
  (every #'package-installed-p mkmcc-packages))

(defun mkmcc-install-packages ()
  "Install all packages listed in `mkmcc-packages'."
  (unless (mkmcc-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (mapc #'package-install
          (remove-if #'package-installed-p mkmcc-packages))))

(mkmcc-install-packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mkmcc-packages)
