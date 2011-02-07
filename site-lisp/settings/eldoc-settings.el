(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(mapc (lambda (hook) (add-hook hook 'turn-on-eldoc-mode))
      '(lisp-interaction-mode-hook 
        emacs-lisp-mode-hook 
        ielm-mode-hook
        scheme-mode-hook))
