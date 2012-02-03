(eval-after-load 'doc-mode
  '(progn
     (add-to-list 'auto-mode-alist '("\\.[Aa][Dd][Oo][Cc]$" . doc-mode))
     (add-to-list 'auto-mode-alist '("\\.[Aa][Ss][Cc][Ii][Ii][Dd][Oo][Cc]$" . doc-mode))))
(require 'asciidoc nil 'noerror)
(require 'doc-mode)
