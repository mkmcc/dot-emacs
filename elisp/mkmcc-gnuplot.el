;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gnuplot scripts
(autoload 'gnuplot-mode "gnuplot-mode.el")

(setq auto-mode-alist
  (append '(("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode))
          auto-mode-alist))

(provide 'mkmcc-gnuplot)
