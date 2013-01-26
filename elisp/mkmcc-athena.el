;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; athena-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a custom mode for athena input files
(defcustom athena-mode-hook nil
  "*Hook to be run when `athena-mode' is entered."
  :type  'hook)

(defcustom athena-indent-offset 0
  "Number of spaces to indent lines in athena mode"
  :type 'integer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set the font lock
(defvar athena-font-lock-keywords
  (list
    ;; Comments
    '("^#.*$" . font-lock-comment-face)
    '("#.*$" . font-lock-comment-face)

    ;; Variable defs
    '("^\\([a-zA-Z0-9_]+\\)[ \t]*=[ \t]*\\([a-zA-Z]+[a-zA-Z0-9_\'\"\-\.]*\\)[ \t]*" .
      ((1 font-lock-keyword-face)
       (2 font-lock-string-face)) )

    '("^\\([a-zA-Z0-9_]+\\)[ \t]*=[ \t]*\\([-0-9.eE]+\\)" .
      ((1 font-lock-keyword-face)
       (2 font-lock-constant-face)) )

    '("^\\(\<\\w+\>\\)[ \t]*" .
      ((1 font-lock-type-face)) ) )
  "Info for function `font-lock-mode'.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define the mode
(defun athena-mode ()
  "Major mode for editing athena files"
  (interactive)
  ;;
  (kill-all-local-variables)
  (setq major-mode 'athena-mode
        mode-name "ATHENA")
  ;;
  (setq comment-start "#")
  (setq comment-start-skip "#+ *")
  ;;
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(athena-font-lock-keywords))
  ;;
  (run-hooks 'athena-mode-hook))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set it to recognize athinput files
(add-to-list 'auto-mode-alist '("\\athinput$" . athena-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-athena)
