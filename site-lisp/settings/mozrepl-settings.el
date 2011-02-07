(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(defun mozrepl-custom-setup ()
  (moz-minor-mode 1))
(add-hook 'espresso-mode-hook 'mozrepl-custom-setup)
(add-hook 'js2-mode-hook 'mozrepl-custom-setup)
