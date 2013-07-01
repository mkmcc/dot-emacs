(after-load 'shell
  (defun comint-delchar-or-eof-or-kill-buffer (arg)
    (interactive "p")
    (if (null (get-buffer-process (current-buffer)))
        (kill-buffer)
      (comint-delchar-or-maybe-eof arg)))

  (define-key shell-mode-map
    (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer))

(provide 'mkmcc-shell)
