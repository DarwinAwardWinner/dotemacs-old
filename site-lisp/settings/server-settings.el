(require 'server)

(defun server-file-name (&optional name)
  "Return the server file name.

Copied from code in `server-force-delete'."
  (expand-file-name (or name server-name)
                    (if server-use-tcp
                        server-auth-dir
                      server-socket-dir)))

(defun server-force-start ()
  "Remove the existing server socket, if any, then start the server.
Use with caution."
  (interactive)
  (server-force-delete)
  (server-start))

;; When running as a daemon, start the server, forcibly blowing away
;; any previous server that may have been running. If not running as a
;; daemon, just start the server if it isn't already started.
(add-hook 'after-init-hook
          (lambda ()
            (if (daemonp)
                (server-force-start)
              (ignore-errors (server-start)))))

(defun server-force-kill ()
  "Kill the server without asking."
  (server-start 'leave-dead 'inhibit-prompt)
  (server-force-delete))

(defun true (&rest stuff)
  "Like `ignore', but returns t."
  t)

(defun save-buffers-kill-emacs-ignore-server (&optional arg)
  (let ((kill-emacs-hook (cons 'server-force-kill kill-emacs-hook)))
    (flet ((server-kill-emacs-query-function (&rest stuff) t))
      (save-buffers-kill-emacs arg))))

(defun save-buffers-kill-something (&optional arg)
  "With no prefix, save-buffers-kill-terminal.
With prefix, save-buffers-kill-emacs.
With double prefix, save-buffers-kill-emacs with prefix.
With negative (or zero) prefix, save-buffers-kill-terminal with prefix."
  (interactive "p")
  (cond
   ((= arg 16) (save-buffers-kill-emacs-ignore-server t)) ; double prefix
   ((= arg 4) (save-buffers-kill-emacs-ignore-server)) ; single prefix
   ((= arg 1) (save-buffers-kill-terminal)) ; no prefix
   ((<= arg 0) (save-buffers-kill-terminal t)) ; neg. prefix
   ))

(global-set-key (kbd "C-x C-c") 'save-buffers-kill-something)

(require 'trash-settings)

;; Any server function that may delete the server file should never
;; move it to trash instead.
(mapc (lambda (fun) (eval `(bypass-trash-in-function ,fun)))
      '(server-start server-sentinel server-force-delete))

(provide 'server-settings)
