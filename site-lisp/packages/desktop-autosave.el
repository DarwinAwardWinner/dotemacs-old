(require 'desktop)

(defun desktop-autosave-save ()
  (when (and desktop-save-mode desktop-dirname)
    (desktop-save-in-desktop-dir)))

(add-hook 'auto-save-hook 'desktop-autosave-save)

(provide 'desktop-autosave)
