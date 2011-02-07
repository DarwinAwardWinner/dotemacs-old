(defun switch-to-active-minibuffer-window ()
  "switch to minibuffer window if active."
  (interactive)
  (let ((minibuf (active-minibuffer-window)))
    (if minibuf
        (select-window (active-minibuffer-window))
      (message "Minibuffer is not currently active."))))

(global-set-key (kbd "<f7>") 'switch-to-active-minibuffer-window)

