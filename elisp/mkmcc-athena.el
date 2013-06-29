;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; athena-mode

(autoload 'athinput-mode "athinput-mode.el")

(add-to-list 'auto-mode-alist '("\\athinput$"     . athinput-mode) t)
(add-to-list 'auto-mode-alist '("\\athinput\\.*$" . athinput-mode) t)


(defun mkmcc-athinput-mode-hook ()
  "defaults for athinput mode"
  (flyspell-prog-mode)
  (prelude-local-comment-auto-fill)
  (prelude-enable-whitespace)
  (prelude-add-watchwords))

(add-hook 'athinput-mode-hook 'prelude-prog-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-athena)
