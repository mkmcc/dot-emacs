(require 'nero)

(defface hackernews-link-face
  '((t (:foreground "green")))
  "Face used for links to articles"
  :group 'hackernews)

(set-face-attribute 'hackernews-link-face nil
                    :foreground nil
                    :inherit 'font-lock-string-face)

(provide 'mkmcc-web)
