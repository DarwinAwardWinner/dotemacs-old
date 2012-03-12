(eval-after-load "ess"
  '(eval-after-load "ido"
     '(disable-ido-ubiquitous-in ess-find-help-file)))

(eval-after-load "ess"
  '(add-hook 'ess-help-mode-hook
             'turn-on-tempbuf-mode))
