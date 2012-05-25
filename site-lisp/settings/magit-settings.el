(require 'magit)
(require 'magit-wip)
(require 'tempbuf)
(require 'cl)
(global-set-key (kbd "C-c C-m") 'magit-status)
(global-set-key (kbd "C-c g") 'magit-status)

(defadvice magit-run* (around use-myinit activate)
  "use git-myinit instead of git-init"
  (let* ((args (ad-get-arg 0))
         (first-two-args (when (>= (length args) 2)
                           (list (first args)
                                 (second args))))
         (remaining-args (when (>= (length args) 2)
                           (cddr args))))
    (when (equal first-two-args '("git" "init"))
      (ad-set-arg 0 (append '("git" "myinit") remaining-args))
      (message "Modified command: %S" (ad-get-arg 0))))
  ad-do-it)

;; Ignore TRAMP errors
(defun magit-revert-buffers (dir &optional ignore-modtime)
  (dolist (buffer (buffer-list))
    (when (and buffer
	       (buffer-file-name buffer)
               (prog1 1 (message "Checking file %s" (buffer-file-name buffer)))
	       (magit-string-has-prefix-p (buffer-file-name buffer) dir)
	       (not (buffer-modified-p buffer))
               (prog1 t (message "Actually checking file %s" (buffer-file-name buffer)))
               (or ignore-modtime (ignore-errors (not (verify-visited-file-modtime buffer))))
               (ignore-errors (file-readable-p (buffer-file-name buffer))))
      (with-current-buffer buffer
	(condition-case var
            (revert-buffer t t nil)
	  (error (let ((signal-data (cadr var)))
		   (cond (t (magit-bug-report signal-data))))))))))

(defadvice magit-run* (after enable-tempbuf activate)
  "Enable `tempbuf-mode' in `magit-process-buffer-name'"
  (let ((buf (get-buffer magit-process-buffer-name)))
    (when buf
      (with-current-buffer buf
        (tempbuf-mode 1)))))

(defmacro magit-define-command (name command &optional force)
  """Define a function called `magit-NAME' that calls (magit-git-command COMMAND)."
  (let ((magit-command-name (intern (concat "magit-" (symbol-name name)))))
    (if (and (not force) (fboundp magit-command-name))
        (error "Function %s is already defined." magit-command-name)
      `(defun ,magit-command-name ()
         ,(format "Run \"git %s\"" (eval command))
         (interactive)
         (magit-git-command ,(eval command))))))

(unless (functionp 'magit-wip-clean)
  (magit-define-command wip-clean "wip-clean"))
