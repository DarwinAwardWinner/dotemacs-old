(eval-after-load "ess"
  '(eval-after-load "ido"
     '(disable-ido-ubiquitous-in ess-find-help-file)))

(eval-after-load "ess"
  '(add-hook 'ess-help-mode-hook
             'turn-on-tempbuf-mode))

;; Filladapt and ess break each other.
(eval-after-load "ess"
  '(load "fill"))
(eval-after-load "filladapt"
  '(when (featurep 'ess)
     (load "fill")))
