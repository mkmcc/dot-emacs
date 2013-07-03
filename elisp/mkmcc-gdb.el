;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GDB customization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom window layout
(defvar gdb-many-windows)
(defvar gdb-use-separate-io-buffer)

(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)

(defadvice gdb-setup-windows (after mkmcc-gdb-setup-windows-advice
                                    first nil
                                    activate)
  "Custom layout for `gdb-many-windows'."
  (interactive)

  ;; source buffer is full height and 0.4 * window width
  (switch-to-buffer (gud-find-file gdb-main-file))
  (delete-other-windows)
  (split-window-horizontally (/ (* (window-width) 2) 5) )

  ;; comint buffer is 0.3 * w and 0.75 * h
  ;; breakpoints is at the bottom
  (other-window 1)
  (pop-to-buffer gud-comint-buffer)
  (split-window-horizontally)
  (split-window nil (/ (* (window-height) 3) 4))

  (when gdb-use-separate-io-buffer
    (split-window nil (/ (* (window-height) 4) 5))
    (other-window 1)
    (gdb-set-window-buffer
     (gdb-get-buffer-create 'gdb-inferior-io)))

  (other-window 1)
  (gdb-set-window-buffer (gdb-breakpoints-buffer-name))


  ;; locals is 0.3 * w and 0.75 * h
  ;; stack is at the bottom
  (other-window 1)
  (gdb-set-window-buffer (gdb-locals-buffer-name))
  (split-window nil ( / (* (window-height) 3) 4))
  (other-window 1)
  (gdb-set-window-buffer (gdb-stack-buffer-name))

  ;; finally, switch to the comint buffer
  (switch-to-buffer gud-comint-buffer) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-gdb)
