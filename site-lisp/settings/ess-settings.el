(eval-after-load 'ess
  '(eval-after-load 'ido
     '(defadvice ess-find-help-file (around disable-ido activate)
        "Disable ido in ess-find-help-file"
        (let ((ido-everywhere nil)) ad-do-it))))



