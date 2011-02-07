(require 'cl)

(defcustom backup-file-exclusion-regexps nil
  "List of regexps for which matching files will not be backed up.
Useful for preventing backups of various files that emacs or
add-on packages automatically manage."
  :group 'backup
  :type '(repeat (regexp :tag "Regexp")))

(defsubst file-excluded-from-backup-p (file)
  "Returns t if file matches one of `backup-file-exclusion-regexps'."
  (find-if (lambda (x) (string-match-p x file))
           backup-file-exclusion-regexps))

(defun my-backup-enable-predicate (name)
  "A custom backup-enable-predicate to use exclusion regexps."
  (or (normal-backup-enable-predicate name)
      (not (file-excluded-from-backup-p (buffer-file-name (current-buffer))))))

(setq backup-enable-predicate 'my-backup-enable-predicate)
