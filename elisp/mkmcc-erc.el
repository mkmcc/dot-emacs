;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up an irc client
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal accounts
(defvar erc-nick "mkmcc")               ;TODO: read this from another file?
(defvar erc-pass "bears")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; convenient settings
(defvar erc-interpret-mirc-color t)
(defvar erc-kill-buffer-on-part t)
(defvar erc-kill-queries-on-quit t)
(defvar erc-kill-server-buffer-on-quit t)

(erc-truncate-mode 1)
(erc-spelling-mode 1)

(defvar erc-server-coding-system  '(utf-8 . utf-8))


;; Make C-c RET (or C-c C-RET) send messages instead of RET.
(defvar erc-mode-map)
(define-key erc-mode-map (kbd "RET") nil)
(define-key erc-mode-map (kbd "C-c RET") 'erc-send-current-line)
(define-key erc-mode-map (kbd "C-c C-RET") 'erc-send-current-line)

;;; logging
(defvar erc-log-channels-directory "~/.erc/logs/")

(if (not (file-exists-p erc-log-channels-directory))
    (mkdir erc-log-channels-directory t))


;;; macs don't have gnutls...
(defvar tls-program
  '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Berkeley-specific things
(defvar erc-autojoin-channels-alist '(("newton.cx" "#badchat")))

(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "Do you want to start IRC? ")
    (erc-tls :server   "newton.cx"
             :port     1420
             :nick     erc-nick
             :password erc-pass)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extirpate irc
(defun filter-server-buffers ()
  (delq nil
        (mapcar
         (lambda (x) (and (erc-server-buffer-p x) x))
         (buffer-list))))

(defun stop-irc ()
  "Disconnects from all irc servers"
  (interactive)
  (dolist (buffer (filter-server-buffers))
    (message "Server buffer: %s" (buffer-name buffer))
    (with-current-buffer buffer
      (erc-quit-server ""))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mkmcc-erc)
