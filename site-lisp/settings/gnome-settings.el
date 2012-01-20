;; http://www.florian-diesch.de/doc/emacs/add-to-gnomes-recently-used-documents/
(defun fd-add-file-to-recent ()
  (when buffer-file-name
    (ignore-errors
      (start-process "/home/ryan/bin/gnome-addtorecent" nil "gnome-addtorecent"
                     (concat "file://" buffer-file-name)
                     "text/plain"
                     "Emacs"
                     "ec -n %F"))))
(add-hook 'find-file-hook 'fd-add-file-to-recent)
