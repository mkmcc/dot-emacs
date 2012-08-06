;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;

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
      media=\"screen, projection\"/>
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"css/print.css\"
      media=\"print\"/>")

;; unnecessary
(setq org-export-html-style-include-scripts nil
      org-export-html-preamble              nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Publish my website
;; Publish using M-x org-publish-project
(defvar website-static-extensions
  '("css" "js" "png" "gif" "pdf" "nb" "c" "rb" "m" "awk" "gpg" "asc")
  "Extensions of static files to copy for my website.")

(defvar website-static-extensions-regexp
  (regexp-opt-group
   (sort website-static-extensions 'string<)))

(setq org-publish-project-alist
      `(("website-org-files"
         :base-directory       "~/Documents/Website/src"
         :base-extension       "org"
         :publishing-directory "~/Documents/Website/site/"
         :recursive            t
         :publishing-function  org-publish-org-to-html
         :headline-levels      4
         :auto-preamble        t
         :auto-sitemap         t
         :sitemap-filename     "sitemap.org"
         :sitemap-title        "sitemap")
        ("website-static"
         :base-directory       "~/Documents/Website/src"
         :publishing-directory "~/Documents/Website/site/"
         :recursive            t
         :publishing-function  org-publish-attachment
         :base-extension       ,website-static-extensions-regexp
         )
        ("website"
         :components ("website-org-files" "website-static"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-org)
