(when (require 'js nil 'noerror)
  (mapc (lambda (c)
          "Replace js2-mode with espresso-mode."
          (when (eq (cdr c) 'js2-mode)
            (setcdr c 'js-mode)))
        auto-mode-alist))
