;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web browsing with lynx
;;
(require 'nero)

;;; look up in ADS
(nero-defelvis "Nasa ADS"
  "http://adsabs.harvard.edu/cgi-bin/nph-basic_connect?qsearch="
  "%20"
  "&version=1")

(defun nero-ads-copy-bibtex ()
  "Extract the bibtex definition from a nero buffer
and save it to the kill ring."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\\(@\\w+\\)")
    (let ((front (match-string-no-properties 1))
          (bpoint (point)))
      (forward-sexp 1)
      (kill-new (concat
                 front
                 (buffer-substring-no-properties bpoint (point)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-web)
