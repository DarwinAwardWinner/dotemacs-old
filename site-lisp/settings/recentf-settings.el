;; http://www.xsteve.at/prg/emacs/power-user-tips.html - recentf
;; Use M-x customize-group recentf
(require 'ido)
(require 'recentf)

(global-set-key (kbd "<f12>") 'recentf-open-files)

;; From http://www.emacswiki.org/emacs/recentf-ext.el:
;;; [2009/03/01] (@* "`recentf' as most recently USED files")
(defun recentf-push-buffers-in-frame ()
  (walk-windows
   (lambda (win)
     (let ((bfn (buffer-local-value 'buffer-file-name (window-buffer win))))
       (and bfn (recentf-add-file bfn))))))
(add-to-list 'window-configuration-change-hook 'recentf-push-buffers-in-frame)
