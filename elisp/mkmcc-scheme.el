;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programming with scheme

;;; this is necessary for using mit-scheme
(setenv "MITSCHEME_LIBRARY_PATH"
        "/Applications/mit-gnu-scheme.app/Contents/Resources")

;;; Note: the documentation says you should pass the -emacs option,
;;; but in my experience this doesn't work.  Maybe it has features
;;; (when combined with xscheme.el??) that I will eventually want, but
;;; for now things work just fine without it.
(setq scheme-program-name "scheme-mech")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fin
(provide 'mkmcc-scheme)
