;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;

;(require 'org-install)
;(require 'org-publish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic setup
(add-to-list 'auto-mode-alist
             '("\\.org$" . org-mode))        ; recognize .org files

(setq org-directory "~/org/")                ; base for my org files

(setq org-default-notes-file
      (concat org-directory
              "notes.org"))                  ; default notes file

(setq org-completion-use-ido t               ; use ido when it makes sense
      org-return-follows-link t              ; return follows the link
      org-use-fast-todo-selection t          ; fast todo selection
      org-log-done 'time                     ; log time when marking as DONE
      org-enforce-to-checkbox-dependencies t ; parents can't be closed...
      org-enforce-todo-dependencies t)       ; ...before their children


(defun mkmcc-org-mode-hook ()
  (electric-indent-mode -1))

(add-hook 'org-mode-hook 'mkmcc-org-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda
(setq org-agenda-files
      (directory-files (concat org-directory "agenda/")
                       t  "^[^#].*\\.org$")    ; ignore backup files
      org-agenda-show-all-dates t              ; shows days without items
      org-agenda-skip-deadline-if-done  t      ; don't show in agenda...
      org-agenda-skip-scheduled-if-done t      ; ...when done
      org-agenda-start-on-weekday nil)         ; start agenda view with today
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
(setq org-hide-leading-stars t                 ; hide leading stars
      org-tags-column -77)                     ; tags at pos 77
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export
(setq org-export-with-section-numbers nil      ; no numbers in export headings
      org-export-with-toc nil                  ; no ToC in export
      org-export-with-author-info nil          ; no author info in export
      org-export-with-creator-info nil         ; no creator info
      org-export-with-LaTeX-fragments t        ; use LaTeX for equations in html
      org-export-htmlize-output-type "css")    ; use css (more flexible)

;; add validator links with images
(setq org-export-html-validation-link
      "<p class=\"validation\">
     <a href=\"http://validator.w3.org/check?uri=referer\">
       <img src=\"http://w3.org/Icons/valid-xhtml10\"
            alt=\"Valid XHTML 1.0 Strict\" height=\"31\" width=\"88\" />
     </a>
     <a href=\"http://jigsaw.w3.org/css-validator/check?uri=referer\">
       <img style=\"border:0; width:88px; height:31px\"
            src=\"http://jigsaw.w3.org/css-validator/images/vcss-blue\"
            alt=\"Valid CSS\" />
     </a>
   </p>")

;; use my own css
(setq org-export-html-style-include-default nil)
(defconst org-export-html-style
  "<link rel=\"stylesheet\" type=\"text/css\"
      href=\"css/screen.css\"
      media=\"screen, projection\">
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"css/print.css\"
      media=\"print\">")

;; unnecessary
(setq org-export-html-style-include-scripts nil
      org-export-html-preamble              nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; TODO States
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "STARTED(s)" "DEFERRED(f)" "|" "DONE(d)")
;;         (sequence "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(i)")
;;         (sequence "|" "CANCELED(c)")))

;; (setq org-todo-keyword-faces
;;       '(("TODO"      . org-warning)
;;         ("STARTED"  . shadow)
;;         ("DEFERRED"  . shadow)
;;         ("KNOWNCAUSE"  . shadow)
;;         ("CANCELED"  . (:foreground "blue" :weight bold))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Publish my website
;; Publish using M-x org-publish-project
(setq org-publish-project-alist
      '(("website-org-files"
         :base-directory "~/public_html/"
         :base-extension "org"
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4
         :auto-preamble t
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         :sitemap-title "sitemap")
        ("website"                      ;can include static files here
         :components ("website-org-files"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-org)
