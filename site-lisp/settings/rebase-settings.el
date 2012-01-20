;; Disable delete-trailing-whitespace in read-only mode, so rebase-mod

(defun rebase-mode-munge-before-save-hook ()
  (set (make-local-variable 'before-save-hook)
       (mapcar (lambda (fun)
                 `(lambda (&rest args)
                    (ignore-errors
                      (apply ,fun args))))
               before-save-hook)))

(add-hook 'rebase-mode-hook 'rebase-mode-munge-before-save-hook)
