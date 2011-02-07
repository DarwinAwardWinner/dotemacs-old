;; (require 'protbuf)
;; ;;(require 'protbuf-by-name)
;; (require 'misc-cmds)

;; (defun kill-protected-buffer (&optional buffer-or-name)
;;   "Kill a buffer, overriding the protection granted by protbuf.el"
;;   (interactive (list (get-buffer (read-buffer "Kill buffer: " (current-buffer) 'existing))))
;;   (message "Killing buffer %s, overriding protections."
;;            (buffer-name buffer-or-name))
;;   (protect-buffer-from-kill-mode nil buffer-or-name)
;;   ;; Also disable by-name protection
;;   (let ((protect-buffer-names) (protect-buffer-matches)
;;         (protect-process-buffer-names) (protect-process-buffer-matches))
;;     (kill-buffer buffer-or-name)))

;; ;; (defadvice kill-buffer-and-its-windows (around protbuf-kill-windows activate)
;; ;;   "Set a flag that lets the other advice know that we want to kill the windows too"
;; ;;   (let ((protbuf-want-to-kill-windows-p t))
;; ;;     (ad-do-it)))

;; ;; (defadvice protect-buffer-from-kill (after protbuf-kill-windows activate)
;; ;;   "If the kill happened due to kill-buffer-and-its-windows,
;; ;; kill the windows even though the buffer isn't being killed."
;; ;;   (when (and (not ad-return-value)
;; ;;              (bound-and-true-p protbuf-want-to-kill-windows-p)
;; ;;              protect-buffer-bury-p)
;; ;;     (dolist (win (get-buffer-window-list (current-buffer) nil t))
;; ;;       (when (window-live-p win) (delete-window win)))))
