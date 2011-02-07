;; Now frames opened by woman will go away when woman is closed
(eval-after-load 'woman
  '(defadvice woman-really-find-file (after set-window-dedicated-p activate)
     "Make the woman window dedicated, so its frame dies on quit."
     (when (and woman-use-own-frame (eq major-mode 'woman-mode))
       (set-window-dedicated-p (selected-window) 'dedicated))))
