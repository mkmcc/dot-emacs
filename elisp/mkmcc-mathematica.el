;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematica
;;

(load-file "~/.emacs.d/vendor/mathematica.el")  ; FIXME

(if mkmcc-macosx-p
    (setq mathematica-command-line
          "/Applications/Mathematica.app/Contents/MacOS/MathKernel")
    (setq mathematica-command-line
          "/apps3/linux-apps/mathematica/math"))

(setq mathematica-never-start-kernel-with-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-mathematica)
