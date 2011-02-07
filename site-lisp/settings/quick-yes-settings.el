(require 'quick-yes)

;; Other option: Use Ido
;; (defadvice yes-or-no-p (around use-ido activate)
;;   "Use Ido for reading yes or no."
;;   (let ((full-prompt (concat prompt "(yes or no)")))
;;     (setq ad-return-value (string= "yes" (ido-completing-read full-prompt '("yes" "no") nil 'require-match)))))

