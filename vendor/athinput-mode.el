;;; athena-mode.el ---

;; Copyright (c) 2013 Mike McCourt (mkmcc@berkeley.edu)
;;

;;; Commentary:

;; A major mode for editing athena files.  Provides basic indentation
;; and syntax highlighting.

;;; Code:

(require 'conf-mode)

(defcustom athinput-mode-hook nil
  "*Hook to be run when `athinput-mode' is entered."
  :type  'hook
  :group 'athinput)

(defun athena-indent-line ()
  "indent the current line"
  (interactive)
  (let ((old-point (point-marker)))
    (beginning-of-line)
    (delete-horizontal-space)
    (goto-char (marker-position old-point))))

(defun athena-align-to-equals ()
  "align all parameter definitions"
  (interactive)
  (save-excursion
    (let ((beg (if (region-active-p) (region-beginning) (point-min)))
          (end (if (region-active-p) (region-end)       (point-max))))
      (align-regexp beg end "\\(\\s-*\\) =" 1 1))))

(defvar athena-mode-syntax-table
  (let ((table (make-syntax-table conf-unix-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    table)
  "syntax table to use in athinput buffers")

(defvar athena-mode-font-lock-keywords
  `(
    ;; blocks
    ("^\\(<\\)\\(\\sw+\\)\\(>\\)"
     (2 'font-lock-type-face))
    ;; numbers
    (,(format "=\\s-*%s\\s-*"
              (regexp-opt '("yes" "no" "true" "false" "on" "off") 'words))
     (1 'font-lock-constant-face))
    ("=\\s-*\\([0-9\\.eE-]+\\)\\s-*"
     (1 'font-lock-constant-face))
    ;; parameter definitions
    ("^\\s-*\\(\\b\\w+\\b\\)\\s-*=\\s-*\\(.*\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-string-face)))
  "font lock extensions for athinput files")

;;;###autoload
(define-derived-mode athinput-mode conf-unix-mode "Athinput"
  "A major mode for editing athinput files."
  (conf-mode-initialize "#" athena-mode-font-lock-keywords)
  (setq indent-tabs-mode nil)
  (set (make-local-variable 'indent-line-function)
       'athena-indent-line)
  (set-syntax-table athena-mode-syntax-table)
  (add-hook 'before-save-hook 'athena-align-to-equals nil t)
  (run-hooks 'athinput-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\athinput$"   . athinput-mode))
(add-to-list 'auto-mode-alist '("\\athinput.*$" . athinput-mode))

(provide 'athinput-mode)

;;; athinput-mode.el ends here
