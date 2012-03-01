(eval-after-load "executable"
  '(progn
     (defadvice executable-make-buffer-file-executable-if-script-p (around skip-backup-files activate)
       "Don't operate on backup files or auto-save files."
       (unless (or (auto-save-file-name-p (buffer-file-name))
                   (backup-file-name-p (buffer-file-name)))
         ad-do-it))

     (defadvice executable-make-buffer-file-executable-if-script-p (around skip-tramp-files activate)
       "Don't operate on remote files."
       (unless (tramp-tramp-file-p (buffer-file-name))
         ad-do-it))

     (defadvice executable-make-buffer-file-executable-if-script-p (around noerror activate)
       "Don't complain if it fails."
       (ignore-errors ad-do-it))))
