;;; Sepia mode (Part of Simple Emacs Perl InterAction),
;; derived from cperl-mode, so replaces it

;; Require these first, so that my alias below overwrites any aliases they
;; might set up.
(require 'perl-mode)
(require 'cperl-mode)
(require 'sepia)
(defalias 'perl-mode 'sepia-mode)

;; Don't use sepia's special TAB functionality
(define-key sepia-mode-map (kbd "TAB") nil)
