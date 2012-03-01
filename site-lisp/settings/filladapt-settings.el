(eval-after-load "filladapt"
  ;; Change function to ignore more arguments
  '(defun fill-paragraph (arg &rest ignored)
     "Fill paragraph at or after point.  Prefix arg means justify as well.

(This function has been overloaded with the `filladapt' version.)

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there.

If `fill-paragraph-function' is non-nil, we call it (passing our
                                                             argument to it), and if it returns non-nil, we simply return its value."
  (interactive "*P")
  (let ((filladapt-inside-filladapt t))
    (filladapt-fill-paragraph 'fill-paragraph arg))))
