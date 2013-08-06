;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package manager
;;
(require 'cl)                           ; can't use dash yet!
(require 'package)
(defvar base-dir)

;; repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-user-dir (expand-file-name "elpa" base-dir))
(package-initialize)

;; automatically update or install some packages
(defvar mkmcc-packages
  '(melpa
    org deft
    gnuplot-mode bibslurp
    rainbow-mode flycheck
    expand-region smart-forward
    volatile-highlights diminish nyan-mode
    paredit elisp-slime-nav ack-and-a-half
    gitconfig-mode gitignore-mode git-commit-mode magit
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
