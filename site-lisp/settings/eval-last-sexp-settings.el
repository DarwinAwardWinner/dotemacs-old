(defun eval-and-replace ()
  "Evaluate the sexp at point and replace it with its value"
  (interactive)
  (let ((value (eval-last-sexp nil)))
    (kill-sexp -1)
    (insert (format "%S" value))))
