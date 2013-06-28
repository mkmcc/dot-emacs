;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; latex mode tweaks
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUCTeX configuration
(defvar TeX-auto-save)
(defvar TeX-parse-self)
(defvar TeX-PDF-mode)
(defvar TeX-view-program-selection)
(defvar TeX-view-program-list)

(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master nil)

;; use pdflatex
(setq TeX-PDF-mode t)

(setq TeX-view-program-selection
      '((output-dvi  "DVI Viewer")
        (output-pdf  "PDF Viewer")
        (output-html "HTML Viewer")))

;; this section is good for OS X only
(setq TeX-view-program-list
      '(("DVI Viewer"  "open %o")
        ("PDF Viewer"  "open %o")
        ("HTML Viewer" "open %o")))

;; add rubber as an option in the compile menu
(eval-after-load "tex"
  '(progn
     (add-to-list 'TeX-expand-list
                  '("%(RubberPDF)"
                    (lambda ()
                      (if (not TeX-PDF-mode)
                          ""
                        "--pdf"))))
     (add-to-list 'TeX-command-list
                  '("Rubber" "rubber %(RubberPDF) %t" TeX-run-shell nil t) t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; locate non-ascii characters
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make rubber the default method for compiling.  bind f9 to compile
;;; and f12 to view.
(defvar TeX-mode-map)
(defvar TeX-command-default)

(defun mkmcc-latex-mode-hook ()
  (define-key TeX-mode-map (kbd "<f9>")
    (lambda ()
      (interactive)
      (save-buffer)
      (TeX-command-menu "Rubber")
      (TeX-clean)))
  (define-key TeX-mode-map (kbd "<f12>")
    (lambda ()
      (interactive)
      (TeX-view)
      [return]))
  (setq TeX-command-default '"Rubber"))

(add-hook 'LaTeX-mode-hook 'mkmcc-latex-mode-hook)
(add-hook 'LaTeX-mode-hook 'abbrev-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mkmcc-latex)
