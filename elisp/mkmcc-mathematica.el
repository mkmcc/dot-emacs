;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematica
;;
(defvar mkmcc-macosx-p)
(defvar mathematica-command-line
  (if mkmcc-macosx-p
      "/Applications/Mathematica.app/Contents/MacOS/MathKernel"
    "/apps3/linux-apps/mathematica/math"))
(defvar mathematica-never-start-kernel-with-mode t)

(load-file "~/.emacs.d/vendor/mathematica.el")  ; FIXME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-mathematica)
