(defun ess-kill-R-help-buffers ()
  "Kill all R help buffers"
  (interactive)
  (mapc (lambda (buf)
          (when (string-begins-with-p (buffer-name buf) "*help[R]") (kill-buffer buf)))
        (buffer-list)))

(require 'auto-complete-acr)
