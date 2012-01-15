;; http://www.xsteve.at/prg/emacs/power-user-tips.html - recentf
;; Use M-x customize-group recentf
(require 'ido)
(require 'recentf)

(global-set-key (kbd "<f12>") 'recentf-open-files)

;; Fix custom Declarations
(eval-after-load 'recentf
  (progn
    (defcustom recentf-max-saved-items 20
      "Maximum number of items of the recent list that will be saved.
A nil value means to save the whole list.
See the command `recentf-save-list'."
      :group 'recentf
      :type '(choice (const :tag "Save whole list" nil)
                     (integer :tag "Save this many" 20)))))

;; From http://www.emacswiki.org/emacs/recentf-ext.el:
;;; [2009/03/01] (@* "`recentf' as most recently USED files")
(defun recentf-push-buffers-in-frame ()
  (walk-windows
   (lambda (win)
     (let ((bfn (buffer-local-value 'buffer-file-name (window-buffer win))))
       (and bfn (recentf-add-file bfn))))))
(add-to-list 'window-configuration-change-hook 'recentf-push-buffers-in-frame)
