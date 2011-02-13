;;; Sepia mode (Part of Simple Emacs Perl InterAction),
;; derived from cperl-mode, so replaces it

(require 'cl)
(require 'sepia)
(defalias 'perl-mode 'sepia-mode)

;; Don't use sepia's special TAB functionality
(define-key sepia-mode-map (kbd "TAB") nil)

;; (let ((perl-modes '(cperl-mode sepia-mode perl-mode)))
;;   (require (car perl-modes))

;;   (loop for alist in `(,auto-mode-alist ,interpreter-mode-alist)
;;         do (mapc (lambda (pair)
;;                    (when (memq (cdr pair)
;;                                perl-modes)
;;                      (message (car pair))
;;                      (setcdr pair 'cperl-mode)))
;;                  alist)))

(defcustom perl-major-modes '(perl-mode cperl-mode sepia-mode)
  "List of major modes that would be used for buffers of Perl code."
  :type '(repeat symbol)
  :group 'perl)

(defun perl-buffer-is-perl-p (&optional buf)
  "Return t if BUF is a perl-mode buffer.

See `perl-major-modes' for modes that are considered valid perl
modes."
  (with-current-buffer (or buf (current-buffer))
    (member major-mode perl-major-modes)))

(defvar perl-module-line-regexp
  "^[[:space:]]*\\(?:use\\|require\\)[[:space:]]+\\([A-Za-z0-9:]+\\)"
  "Regexp that will match a line that uses or requires a Perl module.

After a successful search with this regexp, you can retrieve the
name of the module with `(match-string 1)'.")

(defun perl-next-module (&optional bound)
  "Search forward from point for a perl module name.

Returns the module name if found, else nil. After calling this
function, point will be after the module name.

Optional arg BOUND is a buffer position not to search past."
  (when (re-search-forward perl-module-line-regexp bound 'noerror)
    (match-string 1)))

(defun perl-module-on-current-line (&optional force)
  "Returns the perl module on the current line, if any.

With optional arg FORCE, search for a module on current line even
if the current buffer is not a perl-mode buffer."
  (if (or force (perl-buffer-is-perl-p))
      (save-excursion
        (let ((bound (progn
                       (end-of-line)
                       (point))))
          (beginning-of-line)
          (perl-next-module bound)))
    (message "Current buffer is not a perl-mode buffer.")
    ;; Need to return nil
    nil))

(defun perl-list-modules-in-buffer (&optional buf)
  "Return a list of all perl modules used or required in BUF.

Defaults to current buffer."
  (with-current-buffer (or buf (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let ((modlist ())
            (mod nil))
        (while (setq mod (perl-next-module))
          (when (not (or (member mod modlist)
                         (string-match-p "^v[0-9]+$" mod)))
            (setq modlist (cons mod modlist))))
        modlist))))

(defun perl-list-perl-buffers ()
  "Return a list of perl-mode buffers.

A buffer is considered to be a perl-mode buffer if its major mode
is listed in `perl-major-modes'."
  (remove-if-not 'perl-buffer-is-perl-p (buffer-list)))

(defun perl-list-modules-in-all-perl-buffers ()
  "Return a list of all perl modules used or required in all perl-mode buffers.

A buffer is considered to be a perl-mode buffer if its major mode
is listed in `perl-major-modes'."
  (delete-duplicates (mapcan 'perl-list-modules-in-buffer
                             (perl-list-perl-buffers))
                     :test 'string=))

(defun perl-get-module-completions ()
  "Function to get completions for perl module names.

Returns a list of all perl modules mentioned in any perl-mode
buffer. If the current buffer is a perl-mode buffer, then modules
mentioned in the current buffer are listed first.

A buffer is considered to be a perl-mode buffer if its major mode
is listed in `perl-major-modes'."
  (delete-duplicates (nconc (when (perl-buffer-is-perl-p (current-buffer))
                              (perl-list-modules-in-buffer (current-buffer)))
                            (perl-list-modules-in-all-perl-buffers))
                     :test 'string=))

(defun perl-install-module (modname &rest more-modules)
  "Install a perl module (or modules) using cpanminus."
  (interactive (list
                (completing-read "Module to install: "
                                 (perl-get-module-completions)
                                 nil nil nil nil
                                 (perl-module-on-current-line))))
  (let ((shellbuf (get-buffer "*Async Shell Command*")))
    (when shellbuf
      (or (kill-buffer shellbuf)
          (error "Could not kill previous shell command buffer."))))
  (shell-command (format "cpanm %s &"
                         (mapconcat 'shell-quote-argument
                                    (cons modname more-modules)
                                    " "))))

(defun perl-install-all-modules-for-buffer (&optional buf force)
  "Install all perl modules used by the code in buffer BUF.

Default is current buffer."
  (interactive)
  (if (or force (perl-buffer-is-perl-p buf))
      (perl-install-module (perl-list-modules-in-buffer buf))
    (message "Current buffer is not a perl-mode buffer.")))

(defun perl-install-all-modules-for-all-buffers (&optional buf)
  "Install all perl modules used by the code in all open perl-mode buffers."
  (interactive)
  (perl-install-module (perl-list-modules-in-all-perl-buffers)))

