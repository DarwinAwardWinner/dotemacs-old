(require 'revive+)

(defun recreate-frame (&optional frame)
  (interactive)
  (let* ((old-frame (or frame (selected-frame)))
         (params
          (mapcar (lambda (p)
                    (list p (frame-parameter old-frame p)))
                  '(display display-type title name left top height width fullscreen buffer-list)))
         (window-config (current-window-configuration-printable old-frame))
         (new-frame (make-frame)))
    (mapc (apply-partially 'apply 'set-frame-parameter new-frame)
          params)
    (with-selected-frame new-frame
      (restore-window-configuration window-config))
    (let (x-select-enable-clipboard-manager)
      (delete-frame old-frame))))
