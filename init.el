;; To add settings, put them in ~/.emscs.d/site-lisp/settings/
(setq init-path (expand-file-name "~/.emacs.d/site-lisp"))
(load-file (expand-file-name "init.el" init-path))
(put 'autopair-newline 'disabled nil)
(put 'downcase-region 'disabled nil)
