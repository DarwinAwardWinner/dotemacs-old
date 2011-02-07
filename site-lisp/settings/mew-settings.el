;; ;;(require 'mew)
;; ;; Do not load untile needed
;; (autoload 'mew "mew" nil t)
;; (autoload 'mew-send "mew" nil t)

;; ;; Fix definition of mew-config-alist
;; (defcustom mew-config-alist nil
;;   "*Alist of config. This is a list of
;; 	(<case> (<key> . <value>) (<key> . <value>) ...).
;;   - <case> is a string of case.
;;   - <key> is a string of Mew value with the \"mew-\" prefix removed.
;;   - <value> is a string.

;; Currently, the following keys are supported:
;; \"name\", \"user\", \"mail-domain\",
;; \"cc\", \"fcc\", \"dcc\", \"reply-to\", \"organization\", \"header-alist\",
;; \"proto\",
;; \"smtp-server\", \"smtp-port\", \"smtp-ssh-server\", \"smtp-ssl\", \"smtp-ssl-port\",
;; \"smtp-user\", \"smtp-auth-list\",
;; \"smtp-msgid-user\", \"smtp-msgid-domain\", \"smtp-helo-domain\", \"smtp-mail-from\",
;; \"pop-server\", \"pop-port\", \"pop-ssh-server\", \"pop-ssl\", \"pop-ssl-port\",
;; \"pop-user\", \"pop-auth\", \"pop-auth-list\",
;; \"pop-size\", \"pop-body-lines\", \"pop-delete\", \"pop-body-lines\",
;; \"pop-proxy-server\", \"pop-proxy-port\",
;; \"imap-server\", \"imap-port\", \"imap-ssh-server\", \"imap-ssl\", \"imap-ssl-port\",
;; \"imap-user\", \"imap-auth\", \"imap-auth-list\",
;; \"imap-size\", \"imap-header-only\", \"imap-delete\",
;; \"imap-trash-folder\", \"imap-queue-folder\", \"imap-spam-field\", \"imap-spam-word\",
;; \"imap-proxy-server\", \"imap-proxy-port\",
;; \"nntp-server\", \"nntp-port\", \"nntp-ssh-server\", \"nntp-ssl\", \"nntp-ssl-port\",
;; \"nntp-user\", \"nntp-size\", \"nntp-header-only\",
;; \"nntp-msgid-user\", \"nntp-msgid-domain\",
;; \"ssl-cert-directory\", \"ssl-verify-level\",
;; \"inbox-folder\", \"queue-folder\", \"postq-folder\",
;; \"mailbox-type\", \"mbox-command\", \"mbox-command-arg\",
;; \"signature-file\", \"content-type\", \"refile-guess-alist\",
;; \"spam-prog\", \"spam-prog-args\", \"ham-prog\", \"ham-prog-args\",
;; \"use-old-pgp\".

;; from = name <user@mail-domain>
;; message-id = *random*.smtp-msgid-user@smtp-msgid-domain
;; message-id = *random*.nntp-msgid-user@nntp-msgid-domain

;; An example is as follows:

;; (setq mew-config-alist
;;       '((\"mew\"
;; 	 (\"mail-domain\"  . \"example.org\")
;; 	 (\"inbox-folder\" . \"+inbox-mew\"))
;; 	(\"keio\"
;; 	 (\"cc\"           . \"kazu@example.jp\")
;; 	 (\"user\"         . \"pooh\")
;; 	 (\"mail-domain\"  . \"example.net\"))
;; 	(\"default\"
;; 	 (\"name\"         . \"Kazu Yamamoto\")
;; 	 (\"mail-domain\"  . \"example.jp\"))))
;; "
;;   :group 'mew-env
;;   :type '(alist :key-type string
;;                 :value-type (repeat (cons string sexp))))
