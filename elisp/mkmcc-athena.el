;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; athena-mode

(require 'athinput-mode)

(defun mkmcc-athinput-mode-hook ()
  "defaults for athinput mode"
  (flyspell-prog-mode)
  (prelude-local-comment-auto-fill)
  (prelude-enable-whitespace)
  (prelude-add-watchwords))

(add-hook 'athinput-mode-hook 'prelude-prog-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-athena)
