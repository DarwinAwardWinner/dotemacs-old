(require 'ibuffer)
(defadvice kill-buffer (after update-ibuffer activate)
  (let ((ibuffer-buffer (get-buffer "*Ibuffer*")))
    (when ibuffer-buffer
      (with-current-buffer ibuffer-buffer
        (call-interactively 'ibuffer-update)))))
