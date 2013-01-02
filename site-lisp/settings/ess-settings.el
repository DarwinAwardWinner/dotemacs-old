(eval-after-load "ess"
  '(eval-after-load "ido"
     '(disable-ido-ubiquitous-in ess-find-help-file)))

(eval-after-load "ess"
  '(add-hook 'ess-help-mode-hook
             'turn-on-tempbuf-mode))

;; Filladapt and ess seem to break each other.
(eval-after-load "ess"
  '(load "fill"))
(eval-after-load "filladapt"
  '(load "fill"))

;; Set my own ESS indentation style
(eval-after-load "ess"
  (progn
    (setq ess-default-style 'OWN)
    (setq ess-own-style-list
          (cons (cons 'ess-arg-function-offset-new-line '(4))
                (remove-if
                 (lambda (x)
                   (eq (car x) 'ess-arg-function-offset-new-line))
                 (cdr (assoc 'C++ ess-style-alist)))))))
