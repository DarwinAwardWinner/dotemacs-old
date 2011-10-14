;;; smex - ido-completion for M-x
;; http://github.com/nonsequitur/smex/tree/master

(require 'smex)
(add-hook 'after-init-hook 'smex-initialize)

;; Bind some keys
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; (global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; this is your old M-x
;; Old M-x is bound to <menu>
;;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)



;; Auto update
;;(smex-auto-update 60)
;; (add-hook 'auto-save-hook 'smex-update)

(defmacro update-smex-after (&rest functions)
  "Advise each of FUNCTIONS to execute smex-update upon completion."
  (cons
   'progn
   (mapcar (lambda (fun)
             ;; Running this on `eval' causes an infinite loop, so
             ;; don't do that.
             (when (not (eq fun 'eval))
               `(defadvice ,fun (after smex-update activate)
                  "Run smex-update upon completion"
                  (when (boundp 'smex-cache)
                    (smex-update)))))
           (mapcar 'eval
                   functions))))

(update-smex-after 'load 'eval-last-sexp 'eval-buffer 'eval-region)
