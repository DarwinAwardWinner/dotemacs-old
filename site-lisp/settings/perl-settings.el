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

(defun perl-module-on-current-line ()
  "Returns the perl module on the current line, if any."
  (save-excursion
    (let ((bound (progn
                   (end-of-line)
                   (point))))
      (beginning-of-line)
      (when (re-search-forward "^[[:space:]]*\\(use\\|require\\)[[:space:]]+\\([A-Za-z0-9:]+\\)" bound 'noerror)
        (match-string 2)))))

(defun perl-list-modules-in-buffer (&optional buf)
  "Return a list of all perl modules used or required in BUF

Defaults to current buffer."
  (with-current-buffer (or buf (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let ((modlist ()))
        (while (re-search-forward "^[[:space:]]*\\(use\\|require\\)[[:space:]]+\\([A-Za-z0-9:]+\\)" nil 'noerror)
          (let ((modname (match-string 2)))
            (when (not (or (member modname modlist)
                           (string-match-p "^v[0-9]+$" modname)))
              (setq modlist (cons modname modlist)))))
        modlist))))

(defun perl-list-perl-buffers (&optional modes)
  "Return a list of perl-mode buffers.

Optional arg MODES is a list of additional modes to consider as
valid perl modes."
  (let ((perl-modes (nconc '(perl-mode cperl-mode sepia-mode) modes)))
    (remove-if-not (lambda (buf)
                     (with-current-buffer buf
                       (member major-mode perl-modes)))
                   (buffer-list))))

(defun perl-list-modules-in-all-perl-buffers (&optional modes)
  "Return a list of all perl modules used or required in all perl-mode buffers.

Optional arg MODES is a list of additional modes to consider as
valid perl modes."
  (remove-duplicates (mapcan 'perl-list-modules-in-buffer (perl-list-perl-buffers modes))))

(defun perl-get-module-completions (&optional modes)
  "Function to get completions for perl module names.

Returns a list of all perl modules mentioned in any perl-mode
buffer. If the current buffer is a perl-mode buffer, then modules
mentioned in the current buffer are listed first."
  (remove-duplicates (nconc (when (member major-mode
                                          (nconc '(perl-mode
                                                   cperl-mode
                                                   sepia-mode)
                                                 modes))
                              (perl-list-modules-in-buffer (current-buffer)))
                            (perl-list-modules-in-all-perl-buffers))))

(defun perl-install-module (modname &rest more-modules)
  "Install a perl module (or modules) using cpanminus"
  (interactive (list
                (completing-read "Module to install: "
                                 (perl-get-module-completions)
                                 nil
                                 nil
                                 (perl-module-on-current-line))))
  (shell-command (format "cpanm %s &" (shell-quote-argument modname))))











