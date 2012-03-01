(require 'undo-tree)
(require 'misc-settings)

;; This seems to fix undo-tree keymaps not being set correctly
(eval-after-load "init"
  '(twiddle-mode 'global-undo-tree-mode))

;; M-x (customize-group 'undo-tree)
