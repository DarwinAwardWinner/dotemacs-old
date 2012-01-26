(require 'git-commit)
(defadvice git-commit--save-and-exit (around handle-server-mode activate)
  (if server-buffer-clients
      ;; Server-mode
      (progn
        (save-buffer)
        (server-edit))
    ;; Non-server-mode
    ad-do-it))
