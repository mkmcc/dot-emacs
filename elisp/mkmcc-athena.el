;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; athena-mode

(require 'athinput-mode)

(defun mkmcc-athinput-mode-hook ()
  "defaults for athinput mode"
  (flyspell-prog-mode)
  (prelude-local-comment-auto-fill)
  (prelude-turn-on-whitespace)
  (prelude-add-watchwords)
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))

(add-hook 'athinput-mode-hook 'prelude-prog-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-athena)
