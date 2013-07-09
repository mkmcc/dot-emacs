;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;
(defvar org-completion-use-ido        t)
(defvar org-return-follows-link       t)
(defvar org-cycle-include-plain-lists nil) ; indent, don't fold, lists
(defvar org-src-fontify-natively      t)
(defvar org-hide-leading-stars        t)
(defvar org-tags-column              -77)

(add-auto-mode 'org-mode "\\.org\\'")        ; recognize .org files

(defun mkmcc-org-mode-hook ()
  (electric-indent-mode -1))

(add-hook 'org-mode-hook 'mkmcc-org-mode-hook)

;; update cookies (eg [17/23]) after deleting a line
(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-org)
