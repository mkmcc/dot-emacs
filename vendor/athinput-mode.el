;;; athena-mode.el ---

;; Copyright (c) 2013 Mike McCourt (mkmcc@berkeley.edu)
;;

;;; Commentary:

;; A major mode for editing athena files.  Provides basic indentation,
;; syntax highlighting, and some other nice features.

;;; Code:

(require 'conf-mode)

(defcustom athinput-mode-hook nil
  "Hook to run upon entering `athinput-mode'."
  :type  'hook
  :group 'athinput)

(defun athena-indent-line ()
  "indent the current line"
  (let ((old-point (point-marker)))
    (beginning-of-line)
    (delete-horizontal-space)
    (goto-char (marker-position old-point))))

(defun athena-align-to-equals-globally ()
  "align all parameter definitions in the file"
  (interactive)
  (save-excursion
    (let ((beg (if (region-active-p) (region-beginning) (point-min)))
          (end (if (region-active-p) (region-end)       (point-max))))
      (align-regexp beg end "\\(\\s-*\\) =" 1 1))))

(defun athena-align-to-equals ()
  "align parameter definitions with each block"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<\\sw+>\\([^<]+\\)" nil t)
      (let ((beg (match-beginning 1))
            (end (match-end 1)))
        ;; the below means "find one or more spaces before an equals
        ;; sign and pad that sub-expression so that the equals signs
        ;; line up."
        (align-regexp beg end "\\(\\s-*\\) =" 1 1)))))

(defun athena-add-par-end ()
  "automatically add the block <par_end> if it isn't present"
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\s-*<par_end>" nil t)
      (goto-char (point-max))
      (insert "\n\n<par_end>\n"))))

(defun athena-before-save-hook ()
  (athena-add-par-end)
  (athena-align-to-equals))

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
(define-derived-mode athinput-mode conf-unix-mode "athinput"
  "A major mode for editing athinput files."
  (conf-mode-initialize "#" athena-mode-font-lock-keywords)
  (setq indent-tabs-mode nil)
  (set (make-local-variable 'indent-line-function)
       'athena-indent-line)
  (set-syntax-table athena-mode-syntax-table)
  (add-hook 'before-save-hook 'athena-before-save-hook nil t)
  (run-hooks 'athinput-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\athinput$"     . athinput-mode) t)
(add-to-list 'auto-mode-alist '("\\athinput\\.*$" . athinput-mode) t)

(provide 'athinput-mode)

;;; athinput-mode.el ends here
