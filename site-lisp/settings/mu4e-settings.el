(require 'mu4e)

;; default
;; (setq mu4e-maildir (expand-file-name "~/Maildir"))

(setq mu4e-drafts-folder "/[Gmail]/Drafts")
(setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
(setq mu4e-trash-folder  "/[Gmail]/Trash")

;; don't save message to Sent Messages, Gmail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/[Gmail]/Sent Mail"   . ?s)
         ("/[Gmail]/Trash"       . ?t)
         ("/[Gmail]/All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; See smtp-settings.el for sending mail

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
