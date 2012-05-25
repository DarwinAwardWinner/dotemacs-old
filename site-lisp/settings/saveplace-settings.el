(require 'saveplace)

;; Fix a problem with saveplace.el putting you back in a folded position:
(eval-after-load "org"
  '(add-hook 'org-mode-hook
             (lambda ()
               (when (outline-invisible-p)
                 (save-excursion
                   (outline-previous-visible-heading 1)
                   (org-show-subtree))))))
