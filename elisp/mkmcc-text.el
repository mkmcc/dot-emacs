;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic text editing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unfill paragraph and unfill region
(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  "Replace newline chars in region from START to END by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convenience
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'abbrev-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mkmcc-text)
