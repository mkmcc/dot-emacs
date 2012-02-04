(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)

;;; convenient
(setq erc-interpret-mirc-color t)
(setq erc-kill-buffer-on-part t)
(setq erc-kill-queries-on-quit t)
(setq erc-kill-server-buffer-on-quit t)
(setq erc-query-display 'buffer)

;; logging
(setq erc-log-channels-directory "~/.erc/logs/")

(if (not (file-exists-p erc-log-channels-directory))
    (mkdir erc-log-channels-directory t))

(setq erc-save-buffer-on-part t)
(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t))))

;; truncate long irc buffers
(erc-truncate-mode +1)

;; enable spell checking
(erc-spelling-mode 1)

;; ;; autoaway setup
;; (setq erc-auto-discard-away t)
;; (setq erc-autoaway-idle-seconds 600)
;; (setq erc-autoaway-use-emacs-idle t)

;; utf-8 always and forever
(setq erc-server-coding-system '(utf-8 . utf-8))

(setq tls-program
      '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof
                          -CAfile ~/.private/certs/CAs.pem
                          -cert ~/.private/certs/nick.pem"))

(setq erc-autojoin-channels-alist
      '(("newton.cx" "#badchat")))

(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "Do you want to start IRC? ")
    (erc-tls :server "newton.cx"
             :port 1420
             :nick "mkmcc"
             :password "bears")))

;; (defun filter-server-buffers ()
;;   (delq nil
;;         (mapcar
;;          (lambda (x) (and (erc-server-buffer-p x) x))
;;          (buffer-list))))

;; (defun stop-irc ()
;;   "Disconnects from all irc servers"
;;   (interactive)
;;   (dolist (buffer (filter-server-buffers))
;;     (message "Server buffer: %s" (buffer-name buffer))
;;     (with-current-buffer buffer
;;       (erc-quit-server "Asta la vista"))))

(provide 'prelude-erc)
