;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web browsing with lynx
;;
(require 'nero)

;;; look up in ADS
(nero-defelvis "Nasa ADS"
  "http://adsabs.harvard.edu/cgi-bin/nph-basic_connect?qsearch="
  "%20"
  "&version=1")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-web)
