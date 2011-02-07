;;; Sepia mode (Part of Simple Emacs Perl InterAction),
;; derived from cperl-mode, so replaces it

(require 'sepia)
(defalias 'perl-mode 'sepia-mode)

(add-hook 'sepia-mode-hook (lambda () (local-unset-key (kbd "TAB"))))

;; (let ((perl-modes '(cperl-mode sepia-mode perl-mode)))
;;   (require (car perl-modes))

;;   (loop for alist in `(,auto-mode-alist ,interpreter-mode-alist)
;;         do (mapc (lambda (pair)
;;                    (when (memq (cdr pair)
;;                                perl-modes)
;;                      (message (car pair))
;;                      (setcdr pair 'cperl-mode)))
;;                  alist)))
