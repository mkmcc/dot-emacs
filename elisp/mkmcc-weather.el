;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get a simple weather report
;;
(defvar weather-key "eaf92be513040126122310")
(require 'weather)

(defun weather ()
  (interactive)
  (let ((location (url-hexify-string "94708"))
        (days 3))
    (message "%s"
             (concat
              (weather-report-body
               (weather-json-obj
                (weather-url location days)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-weather)
