(require 'magit)
(require 'tempbuf)
(global-set-key (kbd "C-c C-m") 'magit-status)
(global-set-key (kbd "C-c g") 'magit-status)

(defadvice magit-run* (around use-myinit activate)
  "use git-myinit instead of git-init"
  (when (equal (ad-get-arg 0) (list "git" "init"))
    (ad-set-arg 0 (list "git" "myinit")))
  ad-do-it)

(defface magit-log-head-label-wip
  '((((class color) (background light))
     :box t
     :background "Grey95"
     :foreground "LightSkyBlue3")
    (((class color) (background dark))
     :box t
     :background "Grey07"
     :foreground "LightSkyBlue4"))
  "Face for git-wip labels shown in log buffer."
  :group 'magit-faces)

(defun magit-log-get-wip-color (suffix)
  (list (concat "(WIP) " suffix)
        'magit-log-head-label-wip))

(add-to-list 'magit-refs-namespaces
             '("wip" magit-log-get-wip-color)
             'append)

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

(magit-define-command wip-clean "wip-clean")
