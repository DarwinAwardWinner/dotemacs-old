;; http://www.xsteve.at/prg/emacs/power-user-tips.html - recentf
;; Use M-x customize-group recentf
(require 'ido)
(require 'recentf)

(global-set-key (kbd "<f12>") 'recentf-open-files)

