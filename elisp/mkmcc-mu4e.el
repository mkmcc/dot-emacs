;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure the mu4e email client
;;
(require 'mu4e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic setup
(setq mu4e-maildir        "~/.imap-mail"
      mu4e-attachment-dir "~/Downloads")

(setq mu4e-drafts-folder "/[Gmail].Drafts"
      mu4e-sent-folder   "/[Gmail].Sent Mail"
      mu4e-trash-folder  "/[Gmail].Trash")

(setq mu4e-html2text-command "html2text -b 72")
(setq message-kill-buffer-on-exit t)

(setq user-mail-address "mkmcc@berkeley.edu"
      user-full-name  "Mike McCourt"
      message-signature
      (concat
       "Mike McCourt\n"
       "http://astro.berkeley.edu/~mkmcc\n"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; software
(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 300)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gmail things
(setq mu4e-sent-messages-behavior 'delete) ; GMail handles this.

(setq mu4e-maildir-shortcuts            ; ji for in box, ma to archive
      '( ("/INBOX"               . ?i)
         ("/[Gmail].Sent Mail"   . ?s)
         ("/[Gmail].Trash"       . ?t)
         ("/[Gmail].All Mail"    . ?a)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks
;;
;; make the "from" e-mail address in a reply match the one that the
;;   original was sent to
;;;
;;; TODO: this doesn't work with compose (complains that msg is nil).
;;;
(setq mu4e-compose-pre-hook '())
(add-hook 'mu4e-compose-pre-hook
(defun my-set-from-address ()
  "Set the From address based on the To address of the original."
  (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
        (setq user-mail-address
              (cond
               ((mu4e-message-contact-field-matches msg :to "mkmccjr@gmail.com")
                "mkmccjr@gmail.com")
               ((mu4e-message-contact-field-matches msg :to "mkmcc@astro.berkeley.edu")
                "mkmcc@astro.berkeley.edu")
               (t "mkmcc@berkeley.edu"))))))

(add-hook 'mu4e-compose-mode-hook
        (defun my-do-compose-stuff ()
           "My settings for message composition."
           (set-fill-column 72)
           (flyspell-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-mu4e)
