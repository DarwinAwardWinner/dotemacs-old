;;; Sepia mode (Part of Simple Emacs Perl InterAction),
;; derived from cperl-mode, so replaces it
(require 'sepia)
(defalias 'perl-mode 'sepia-mode)

;; Don't use sepia's special TAB functionality
(define-key sepia-mode-map (kbd "TAB") nil)
