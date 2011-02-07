;; Do general org-remember integration
(org-remember-insinuate)

;; TODO Set this to the same as the system-globalr
(global-set-key (kbd "C-c r") 'org-remember)

(defadvice org-remember-finalize (after redraw-display activate)
  "Redraw the display after finalizing org-remember buffer"
  (redraw-display))

(defun org-remember-require-refile (&optional arg)
  "Require the current note to be refiled manually."
  (interactive "P")
  (set (make-local-variable 'org-remember-store-without-prompt)
       (if arg
           (< arg 1)
         (not org-remember-store-without-prompt)))
  "")

(defcustom remember-frame-alist nil
  "Additional frame parameters for dedicated remember frame."
  :type 'alist
  :group 'remember)

(defadvice remember (around remember-frame-parameters activate)
  "Set some frame parameters for the remember frame."
  (let ((default-frame-alist (append remember-frame-alist
                                     default-frame-alist)))
    ad-do-it))
