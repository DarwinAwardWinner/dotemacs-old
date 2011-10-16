(require 'desktop)
(require 'desktop-autosave)
(require 'defadvice-let)
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (search-forward "emacs" nil t)
          pid)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

;; Don't ask about local variables if we're running as a daemon.
;; Instead, only load safe ones.
(defadvice desktop-create-buffer (around do-not-ask-daemon activate)
  (if (and (daemonp)
           (not (memq enable-local-variables '(nil :safe :all))))
      (let ((enable-local-variables :safe))
        ad-do-it)
    ad-do-it))

(defadvice desktop-read (around avoid-redundant-read activate)
  (if (and (desktop-owner) (= (desktop-owner) (emacs-pid)))
      (message "Desktop file already loaded. Skipping reload.")
    ad-do-it))

;; Load the desktop *after* all init stuff is done
(eval-after-load 'init
  '(progn
     ;; (desktop-read)
     (desktop-save-mode 1)))
