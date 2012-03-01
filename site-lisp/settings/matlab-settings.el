(add-to-list 'auto-mode-alist '("\\.[mM]\\'" . matlab-mode))
(eval-after-load "matlab-mode"
  '(progn
     (define-key matlab-mode-map (kbd "M-;") 'comment-dwim)))
