;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org export
;;
(defvar org-export-with-section-numbers)
(defvar org-export-with-toc)
(defvar org-export-with-author-info)
(defvar org-export-with-creator-info)
(defvar org-export-with-LaTeX-fragments)
(defvar org-export-htmlize-output-type)

(setq org-export-with-section-numbers nil      ; no numbers in export headings
      org-export-with-toc nil                  ; no ToC in export
      org-export-with-author-info nil          ; no author info in export
      org-export-with-creator-info nil         ; no creator info
      org-export-with-LaTeX-fragments t        ; use LaTeX for equations in html
      org-export-htmlize-output-type "css")    ; use css (more flexible)

;; add validator links with images
(defvar org-export-html-validation-link)
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
(defvar org-export-html-style-include-default)
(setq org-export-html-style-include-default nil)
(defconst org-export-html-style
  "<link rel=\"stylesheet\" type=\"text/css\"
      href=\"css/screen.css\"
      media=\"screen, projection\"/>
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"css/print.css\"
      media=\"print\"/>")

;; unnecessary
(defvar org-export-html-style-include-scripts)
(defvar org-export-html-preamble)
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

(defvar org-publish-project-alist)
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

(provide 'mkmcc-org-website)
