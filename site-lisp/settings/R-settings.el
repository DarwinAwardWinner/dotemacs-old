(defun ess-kill-R-help-buffers ()
  "Kill all R help buffers"
  (interactive)
  (mapc (lambda (buf)
          (when (string-begins-with-p (buffer-name buf) "*help[R]") (kill-buffer buf)))
        (buffer-list)))

(eval-after-load 'auto-complete
  '(require 'ac-R))
