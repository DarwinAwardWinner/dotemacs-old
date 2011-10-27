(defmacro defun-lexical (name args &rest function-definition)
  "Like `defun', but always acts as though `lexical-binding' were true."
  (let* ((function-def `(closure (t) ,args ,@function-definition)))
    `(prog1 ',name
       (fset ',name #',function-def))))

;; Make closure work like lambda, but only in Emacs 24 or later.
(when (>= emacs-major-version 24)
  (defmacro make-self-quoting (name)
    "Make NAME into a self-quoting function like `lambda'."
    `(defmacro ,name (&rest cdr)
       (list 'function (cons ',name cdr))))
  (make-self-quoting closure))
