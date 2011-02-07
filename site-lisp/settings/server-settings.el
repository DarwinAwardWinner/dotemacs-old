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

;; ;; Don't start the server unless we can verify that it isn't running.
;; ;; And Don't complain if it doesn't.
;; (when (and (functionp 'server-running-p) (not (server-running-p)))
;;   (ignore-errors (server-start)))
(add-hook 'after-init-hook 'server-force-start)


(defun server-force-kill ()
  "Kill the server without asking."
  (flet ((yes-or-no-p (&rest stuff) t)) (server-mode -1))
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
