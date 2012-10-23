;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; latex mode tweaks
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUCTeX configuration
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
;; functions to locate non-ascii characters
(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
      (message "No non-ascii characters."))))

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unfill paragraph and unfill region
(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  "Replace newline chars in region from START to END by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make rubber the default method for compiling.  bind f9 to compile
;;; and f12 to view.
(defun mkmcc-latex-mode-hook ()
  (turn-on-auto-fill)
  (abbrev-mode +1)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mkmcc-latex)
