;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic setup
(add-auto-mode 'org-mode "\\.org\\'")        ; recognize .org files

(defvar org-directory)
(defvar org-default-notes-file)

(setq org-directory "~/org/")                ; base for my org files

(setq org-default-notes-file
      (concat org-directory
              "notes.org"))                  ; default notes file

(defvar org-completion-use-ido)
(defvar org-return-follows-link)
(defvar org-use-fast-todo-selection)
(defvar org-log-done)
(defvar org-enforce-to-checkbox-dependencies)
(defvar org-enforce-todo-dependencies)
(defvar org-cycle-include-plain-lists)
(defvar org-src-fontify-natively)

(setq org-completion-use-ido t               ; use ido when it makes sense
      org-return-follows-link t              ; return follows the link
      org-use-fast-todo-selection t          ; fast todo selection
      org-log-done 'time                     ; log time when marking as DONE
      org-enforce-to-checkbox-dependencies t ; parents can't be closed...
      org-enforce-todo-dependencies t        ; ...before their children
      org-cycle-include-plain-lists nil      ; indent, not fold, lists
      org-src-fontify-natively t)            ; fontify code in src blocks

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda
(autoload 'org-agenda "org-agenda.el")

(eval-after-load "org-agenda"
  '(progn
     (setq org-agenda-files
           (directory-files (expand-file-name "agenda/" org-directory)
                            t  "^[^#].*\\.org$") ; ignore backup files
           org-agenda-show-all-dates t           ; shows days without items
           org-agenda-skip-deadline-if-done  t   ; don't show in agenda...
           org-agenda-skip-scheduled-if-done t   ; ...when done
           org-agenda-start-on-weekday nil)))    ; start agenda view with today
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
(defvar org-hide-leading-stars)
(defvar org-tags-column)
(setq org-hide-leading-stars t                 ; hide leading stars
      org-tags-column -77)                     ; tags at pos 77
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mkmcc-org)
