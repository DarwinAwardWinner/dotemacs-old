(require 'ebuff-menu)
;; Use electric buffers instead
;; (substitute-key-definition 'list-buffers 'electric-buffer-list global-map)
;; Apparently they forgot to bind x
(define-key electric-buffer-menu-mode-map "x" 'Buffer-menu-execute)
