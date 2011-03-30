(add-to-list 'auto-mode-alist '("\\.?gitignore" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.?gitconfig" . conf-mode))

(require 'magit)
(global-set-key (kbd "C-c C-m") 'magit-status)

(require 'git-wip-mode)
