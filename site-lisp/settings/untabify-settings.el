(defun untabify-buffer ()
  "Untabify the entire buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun tabify-buffer ()
  "Untabify the entire buffer."
  (interactive)
  (tabify (point-min) (point-max)))
