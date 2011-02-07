(eval-after-load 'executable
  '(progn
     (defadvice executable-make-buffer-file-executable-if-script-p (around skip-backup-files activate)
       (unless (or (auto-save-file-name-p (buffer-file-name))
                   (backup-file-name-p (buffer-file-name)))
         ad-do-it))))