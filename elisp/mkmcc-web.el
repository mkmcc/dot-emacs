;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web browsing with lynx
;;
(require 'nero)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADS utils -- this is awesome!
;;
;; search ADS
(nero-defelvis "Nasa ADS"
  "http://adsabs.harvard.edu/cgi-bin/nph-basic_connect?qsearch="
  "%20"
  "&version=1")

(defun nero-ads-copy-bibtex ()
  "Extract the bibtex definition from a nero buffer and save it
to the kill ring.

Only use this function from a page displaying the bibtex entry.
You're better off using `nero-slurp-bibtex', but I made this
interactive anyways.  Just in case you happen to land on the
bibtex page."
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

(defun nero-slurp-bibtex (&optional link-number)
  "Read a link number and save the corresponding bibtex entry to the kill-ring.

The link you provide should point to an ADS \"abstract\" page.
So you'd use this from, e.g. a page listing search results.  This
is awesome."
  (interactive (list (read-string "Link Number: ")))
  (let ((url (nero-follow-link-internal link-number 'return-link)))
    (nero-browse-url url nil nil nil
       (lambda ()
         (cond
          ((search-forward "BibTeX" nil t)
           (nero-move-to-previous-link)
           (nero-follow-link nil nil
                             (lambda ()
                               (nero-ads-copy-bibtex)))
           (message "Copied bibtex entry to kill-ring."))
          (t
           (message "Couldn't find bibtex entry.")))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-web)
