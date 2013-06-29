;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deft customizations
;;
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

(setq deft-directory "~/.emacs.d/deft/") ; keep my ~ clean
                                         ; note trailing / is impt.
(global-set-key [f5] 'deft)
(setq deft-strip-title-regexp "^#\\+\\w+:[ \t]+\\|^[#* ]*")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-deft)
