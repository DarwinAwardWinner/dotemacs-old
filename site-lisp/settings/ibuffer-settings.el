(require 'ibuffer)

(defvar ibuffer-needs-update nil)

(defadvice kill-buffer (after update-ibuffer activate)
  (setq ibuffer-needs-update t)
  (run-with-idle-timer
   0.1 nil
   (lambda ()
     (when ibuffer-needs-update
       (let ((ibuffer-buffer (get-buffer "*Ibuffer*")))
         (when ibuffer-buffer
           (with-current-buffer ibuffer-buffer
             (call-interactively 'ibuffer-update))))
       (setq ibuffer-needs-update nil)))))
