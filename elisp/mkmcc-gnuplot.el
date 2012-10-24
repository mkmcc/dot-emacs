;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gnuplot scripts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable the package
(require 'gnuplot-mode)

(if mkmcc-macosx-p
  (setq gnuplot-program "gnuplot")
  (setq gnuplot-program "/usr/bin/gnuplot"))

(setq auto-mode-alist
  (append '(("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode))
          auto-mode-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-gnuplot)
