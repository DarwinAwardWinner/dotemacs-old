(eval-after-load "tmm"
  '(progn
     (require 'cl)
     ;; Disable ido for tmm-menubar
     (defadvice tmm-menubar (around disable-completion activate)
       "Prevent normal completion from breaking tmm-menubar"
       (let ((old-icomplete-mode icomplete-mode)
             (ido-everywhere nil))
         (icomplete-mode 0)
         ad-do-it
         ;; Reset icomplete-mode
         (icomplete-mode (if old-icomplete-mode 1 0))))))
