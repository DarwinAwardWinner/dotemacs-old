(require 'ido)
(load "ido-other-window" 'noerror)
(when (load "ido-yes-or-no" 'noerror)
  (ido-yes-or-no-mode 1))

(defadvice ido-complete-space (around handle-require-match activate)
  "If require-match is nil, always insert space."
  (if (bound-and-true-p require-match)
      (ido-complete)
    (insert " ")))

(eval-after-load 'mic-paren
  '(defadvice mic-paren-highlight (around disable-inside-ido activate)
     "Disable mic-paren highlighting in ido"
     (unless (ido-active)
       ad-do-it)))

(provide 'ido-settings)
