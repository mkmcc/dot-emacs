;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deft customizations
;;
(defvar deft-extension)
(defvar deft-text-mode)
(defvar deft-directory)
(defvar deft-strip-title-regexp)

(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

(setq deft-directory "~/.emacs.d/deft/") ; keep my ~ clean
                                         ; note trailing / is impt.
(global-set-key [f5] 'deft)
(setq deft-strip-title-regexp "^#\\+\\w+:[ \t]+\\|^[#* ]*")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-deft)
