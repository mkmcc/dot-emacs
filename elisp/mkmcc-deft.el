;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deft customizations
;;; Time-stamp: <2012-02-02 09:50:39 (mkmcc)>
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

(setq deft-directory "~/.emacs.d/deft/") ; keep my ~ clean
                                         ; note trailing / is impt.
(global-set-key [f5] 'deft)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-deft)
