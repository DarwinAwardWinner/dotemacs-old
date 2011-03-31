(mapc '(lambda (hook) (add-hook hook 'turn-on-visual-line-mode))
      '(text-mode-hook
        lisp-interaction-mode-hook))

(with-current-buffer "*scratch*"
  (turn-on-visual-line-mode))
