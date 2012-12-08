(require 'tramp)

;; Turn off auto-save-mode for tramp files
(defun tramp-turn-off-auto-save ()
  (when (tramp-file-name-p (buffer-file-name (current-buffer)))
    (auto-save-mode 0)))
(add-hook 'find-file-hook 'tramp-turn-off-auto-save)

(defun tramp-dissect-file-name-maybe (name &optional nodefault)
  "Like tramp-dissect-file-name, but returns nil instead of error."
  (when (tramp-tramp-file-p name)
    (tramp-dissect-file-name name nodefault)))

(defun toggle-sudo-edit (&optional buf)
  "Toggle BUF between editing as root and normal user.

Default BUF is current buffer. The positions of point and mark
are preserved through the toggle."
  (interactive)
  (let* ((buf (or buf (current-buffer)))
         (old-point (with-current-buffer buf (point)))
         (old-mark (with-current-buffer buf (mark)))
         (filename (buffer-file-name buf))
         (tramp-file-name (tramp-dissect-file-name-maybe filename))
         new-filename)
    (if tramp-file-name
        (let* ((method (aref tramp-file-name 0))
               (user (aref tramp-file-name 1))
               (host (aref tramp-file-name 2))
               (localname (aref tramp-file-name 3)))
          (if (and (string= method "sudo")
                   (string= user "root")
                   (member host '("localhost" (system-name))))
              (setq new-filename localname)
            (error "File %s is a TRAMP file but is not being edited by sudo as root on localhost." filename)))
      (setq new-filename (concat "/sudo:root@localhost:" filename)))
    (find-alternate-file new-filename)
    (goto-char old-point)
    (set-mark old-mark)))

(global-set-key (kbd "C-x C-r") 'toggle-sudo-edit)

;; Enable completion of ssh hosts for all ssh-based methods
(let ((my-tramp-ssh-completions
       '((tramp-parse-sconfig "~/.ssh/config")
         (tramp-parse-shosts "~/.ssh/known_hosts"))))
  (mapc (lambda (method)
          (tramp-set-completion-function method my-tramp-ssh-completions))
        '("fcp" "rsync" "scp" "scpc" "scpx" "sftp" "ssh" "sudo")))

(defun tramp-get-method-parameter (method param)
  "Return the method parameter PARAM.
If the `tramp-methods' entry does not exist, return NIL."
  (let ((entry (assoc param (assoc method tramp-methods))))
    (when entry (cadr entry))))

(defun tramp-set-method-parameter (method param newvalue)
  "Set the method paramter PARAM to VALUE for METHOD.

If METHOD does not yet have PARAM, add it.
If METHOD does not exist, do nothing."
  (let ((method-params (assoc method tramp-methods)))
    (when method-params
      (let ((entry (assoc param method-params)))
        (if entry
            (setcar (cdr entry) newvalue)
          (setcdr (last method-params) '(param newvalue)))))))

(defun tramp-add-args (programs newargs)
  "Append NEWARGS to the argument list for any of PROGRAMS in `tramp-methods'.

PROGRAMS can be a list of strings, or a single string."
  ;; Allow a single program string or a list of matching programs.
  (when (stringp programs)
    (setq programs (list programs)))
  (message "%s" (list programs newargs))
  (loop for method in (mapcar 'car tramp-methods) do
        (let ((login-program (tramp-get-method-parameter method 'tramp-login-program))
              (copy-program (tramp-get-method-parameter method 'tramp-copy-program))
              (login-args (tramp-get-method-parameter method 'tramp-login-args))
              (copy-args (tramp-get-method-parameter method 'tramp-copy-args)))
          (message "Handling %s" method)
          (message "  Handling login program %s" login-program)
          (when (find login-program programs :test 'string=)
            (message "    Adding to login program %s" login-program)
            (tramp-set-method-parameter method 'tramp-login-args (append login-args newargs)))
          (message "  Handling copy program %s" login-program)
          (when (find copy-program programs :test 'string=)
            (message "    Adding to copy program %s" copy-program)
            (tramp-set-method-parameter method 'tramp-copy-args (append copy-args newargs))))))

(tramp-add-args
 '("scp" "scp1" "scp2" "scp1_old" "scp2_old" "sftp" "rsync" "ssh" "ssh1" "ssh2" "ssh1_old" "ssh2_old" "scpx" "sshx")
 '(("-o" "ControlPath=~/.ssh/control/emacs-master-%%r@%%h:%%p" "-o" "ControlMaster=auto")))

(defun ssh-cleanup ()
  (ignore-errors
    (call-process "ssh-cleanup")))

(defadvice tramp-cleanup-all-connections (after cleanup-control-files activate)
  (ssh-cleanup))

(defun .first-three-equal (seq1 seq2 &optional compare-fun)
  (unless compare-fun
    (setq compare-fun 'equal))
  (loop for i from 0 upto 2
        unless (funcall compare-fun (elt seq1 i) (elt seq2 i))
        return nil
        finally (return t)))

(defun tramp-same-server (path1 path2)
  (if (tramp-tramp-file-p path1)
      (if (tramp-tramp-file-p path2)
          ;; Both TRAMP files
          (first-three-equal
           (tramp-dissect-file-name path1)
           (tramp-dissect-file-name path2))
        ;; Only path1 is TRAMP
        nil)
    (if (tramp-tramp-file-p path2)
        ;; Only path2 is TRAMP
        nil
      ;; Neither one is TRAMP
      t)))

(defun tramp-substitute-server-in-variable (var old new)
  "Substitute NEW in place of OLD in VAR.

OLD and NEW should be TRAMP server specifications, like \"ssh:HOSTNAME:\"."
  (let* ((oldvec (tramp-dissect-file-name (concat "/" old ":")))
         (newvec (tramp-dissect-file-name (concat "/" new ":")))
         (value (eval var))
         (valuevec (ignore-errors (tramp-dissect-file-name value))))
    (assert (string= (elt oldvec 3) "") t)
    (assert (string= (elt newvec 3) "") t)
    (when (and (tramp-tramp-file-p value)
               (.first-three-equal oldvec valuevec))
      (set var
           (tramp-make-tramp-file-name
            (elt newvec 0)
            (elt newvec 1)
            (elt newvec 2)
            (elt valuevec 3))))))

(defun tramp-substitute-server (old new)
  "Substitute NEW in place of OLD in `buffer-file-name' and `default-directory' of all buffers.

OLD and NEW should be TRAMP server specifications, like \"ssh:HOSTNAME:\"."
  (interactive "sOld TRAMP server: \nsNew TRAMP server: ")
  (loop for buf in (buffer-list)
        do (with-current-buffer buf
             (mapc (lambda (var) (tramp-substitute-server-in-variable var old new))
                   '(buffer-file-name default-directory)))))

;; For some reason this keeps getting unset
(add-to-list 'tramp-gvfs-methods "sftp")
