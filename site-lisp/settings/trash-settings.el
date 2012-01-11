;;; trash-settings.el - Intelligent integration with system trash

;; Use the system trash, except for temp files and stuff

(require 'cl)

(defcustom system-trash-exclude-names
  nil
  "List of file names to exclude from system trash.

The names in this variable are matched only against the basename
of the file to be deleted."
  :type '(repeat string)
  :group 'trash)

(defcustom system-trash-exclude-paths
  '("/tmp")
  "List of absolute paths to exclude from system trash.

Excluding a directory also excludes all of its contents."
  :type '(repeat string)
  :group 'trash)

(defcustom system-trash-exclude-matches
  nil
  "List of regexps or functions matching file names to exclude from system trash.

The matches are only applied against the file name, not the path."
  :type '(repeat (choice regexp function))
  :group 'trash)

(defcustom system-trash-exclude-path-matches
  nil
  "List of regexps or functions matching paths to exclude from system trash.

The matches are applied against the full path."
  :type '(repeat (choice regexp function))
  :group 'trash)

(defcustom trash-only-inside-home
  nil
  "If t, only move files to trash that are inside your home directory."
  :type 'boolean
  :group 'trash)

(defun call-process-discard-output (program &rest args)
  "Execute program with args without saving any output.
In particular, no temp files are created."
  (eval (append `(call-process ,program nil nil nil) args)))

(defun string-begins-with-p (string beginning)
  "Return t if and only if string begins with beginning"
  (string-match-p (concat "^" (regexp-quote beginning)) string))

(defun file-excluded-from-system-trash-p (path)
  "Returns non-nil if file name is excluded from trash."
  (let ((basename (file-name-nondirectory path)))
    (or
     (some (apply-partially 'string= basename)
           system-trash-exclude-names)
     (some (apply-partially 'string-begins-with-p path)
           system-trash-exclude-paths)
     (some (lambda (match)
             (funcall
              (cond ((stringp match) 'string-match-p)
                    ((functionp protected-match) 'funcall)
                    (t 'ignore))
              match
              basename))
           system-trash-exclude-matches)
     (some (lambda (match)
             (funcall
              (cond ((stringp match) 'string-match-p)
                    ((functionp protected-match) 'funcall)
                    (t 'ignore))
              match
              path))
           system-trash-exclude-path-matches))))

(defun delete-file-or-directory-internal (filename)
  (if (and (file-directory-p filename)
           (not (file-symlink-p filename)))
      (delete-directory filename t nil)
    (delete-file filename nil)))

(defun trash-or-rm (filename)
  "Attempt to move a file to the trash. If this fails, simply delete it.
This guarantees that any deletable file will either be trashed or deleted.
If the file is excluded from the trash, it is simply deleted."
  (unless (file-excluded-from-system-trash-p filename)
    (ignore-errors
      (call-process-discard-output "gvfs-trash" filename)))
  (when (file-exists-p filename)
    (delete-file-or-directory-internal filename)))

(setq trash-directory nil)
(defalias 'system-move-file-to-trash 'trash-or-rm)

(defadvice delete-directory (around no-recursive-trash activate)
  "When trashing a directory, don't trash its contents as well.

Normally, directories are deleted recusrively, which means that
when `delete-by-moving-to-trash' is t, every file and dir within
that directory would be trashed *separately*. That's bad. So this
advice bypasses the normal recursive delete behavior when
trashing. Also, since trashing is undoable, there's no need to
ask the user for confirmation.

Of course, if trashing the directory fails, fall back to normal
behavior."
  (if delete-by-moving-to-trash
      (condition-case ex
          (move-file-to-trash directory)
        ('error ad-do-it))
    ad-do-it))

(defadvice dired-delete-file (around no-recursive-trash activate)
  "Dired doesn't need to ask for confirmation when trashing."
  (if delete-by-moving-to-trash
      (condition-case ex
          (move-file-to-trash file)
        ('error ad-do-it))
    ad-do-it))

(defmacro bypass-trash-in-function (fun)
  "Set FUN to always use normal deletion, and never trash.

Specifically, the value of `delete-by-moving-to-trash' will be
set to nil inside FUN, so any deletions that happen inside FUN or
any functions called by it will bypass the trash."
  `(defadvice ,fun (around no-trash activate)
     "Ignore `delete-by-moving-to-trash' inside this function.

See `bypass-trash-in-function' for more information."
     (let (delete-by-moving-to-trash)
       ad-do-it)))

(provide 'trash-settings)
