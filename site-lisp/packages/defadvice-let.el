(require 'cl)

(defmacro defadvice-let (varlist fn &optional docstring)
  "Wrap FN in a dynamic binding of VARLIST.

DOCSTRING, if provided, becomes the docstring of the advice."
  `(defadvice ,fn (around ,(gensym "defadvice-let") activate)
     ,@(funcall (if docstring
                    (apply-partially 'cons docstring)
                  'identity)
                `((let ,varlist
                    ad-do-it)))))

(provide 'defadvice-let)
