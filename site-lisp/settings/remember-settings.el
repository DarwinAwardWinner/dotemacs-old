(require 'remember)
(require 'defadvice-let)

(defcustom remember-frame-alist nil
  "Additional frame parameters for dedicated remember frame."
  :type 'alist
  :group 'remember)

(defadvice-let ((default-frame-alist (append remember-frame-alist default-frame-alist)))
  remember
  "Set some frame parameters for the remember frame.")

;; (defadvice remember (around remember-frame-parameters activate)
;;   "Set some frame parameters for the remember frame."
;;   (let ((default-frame-alist (append remember-frame-alist default-frame-alist)))
;;     ad-do-it))

;; This restores the window configuration early, in order to more
;; quickly return you to your regularly-scheduled program.
(defun remember-region (&optional beg end)
  "Remember the data from BEG to END.
It is called from within the *Remember* buffer to save the text
that was entered.

If BEG and END are nil, the entire buffer will be remembered.

If you want to remember a region, supply a universal prefix to
`remember' instead.  For example: \\[universal-argument] \\[remember] RET."
  ;; Sacha: I have no idea where remember.el gets this context information, but
  ;; you can just use remember-annotation-functions.
  (interactive)
  (jump-to-register remember-register)
  (save-window-excursion
    (switch-to-buffer remember-buffer)
    (let ((b (or beg (min (point) (or (mark) (point-min)))))
          (e (or end (max (point) (or (mark) (point-max))))))
      (save-restriction
        (narrow-to-region b e)
        (if remember-all-handler-functions
            (run-hooks 'remember-handler-functions)
          (run-hook-with-args-until-success 'remember-handler-functions))
        (kill-buffer remember-buffer)))))

(defun remember-emacsclient-special-function (&optional initial)
  "Only to be called by emacsclient -c -e

The -c option to emacsclient makes a frame. This function makes
it invisible, then calles ``remember-other-frame'', then deletes
the other frame. This is because ``switch-to-buffer-other-frame''
doesn't work right in emacsclient -e unless another frame already
exists."
  (let ((the-frame (selected-frame)))
    (make-frame-invisible the-frame 'force)
    (remember-other-frame initial)
    (delete-frame the-frame)))
