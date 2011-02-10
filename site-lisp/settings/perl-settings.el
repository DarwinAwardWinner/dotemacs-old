;;; Sepia mode (Part of Simple Emacs Perl InterAction),
;; derived from cperl-mode, so replaces it

(require 'sepia)
(defalias 'perl-mode 'sepia-mode)

;; Don't use sepia's special TAB functionality
(define-key sepia-mode-map (kbd "TAB") nil)

;; (let ((perl-modes '(cperl-mode sepia-mode perl-mode)))
;;   (require (car perl-modes))

;;   (loop for alist in `(,auto-mode-alist ,interpreter-mode-alist)
;;         do (mapc (lambda (pair)
;;                    (when (memq (cdr pair)
;;                                perl-modes)
;;                      (message (car pair))
;;                      (setcdr pair 'cperl-mode)))
;;                  alist)))
