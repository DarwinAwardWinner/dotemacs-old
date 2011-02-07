(require 'cl)

(defmacro without-minibuffer (&rest body)
  "Like `progn', but stop and return nil if BODY tries to use the minibuffer.

Also disable dialogs while evaluating BODY forms, since dialogs
are just an alternative to the minibuffer."
  `(catch 'tried-to-use-minibuffer
     (minibuffer-with-setup-hook
         (lambda (&rest args) (throw 'tried-to-use-minibuffer nil))
       (let ((use-dialog-box))          ; No cheating by using dialogs instead of minibuffer
         ,@body))))


;; This should be indented like progn
(put 'without-minibuffer 'lisp-indent-function
     (get 'progn 'lisp-indent-function))

(defmacro without-functions (flist &rest body)
  "Evaluate BODY, but stop and return nil if BODY calls any of the functions named in FLIST."
  (let* (;; Functions are disabled by setting their body to this
         ;; temporarily.
         (fbody
          '((&rest args) (throw 'forbidden-function nil)))
         ;; This will form the first argument to `flet'
         (function-redefinitions
          (mapcar (lambda (fname) (cons fname fbody)) flist)))
    `(catch 'forbidden-function
       (flet ,function-redefinitions
         ,@body))))

(put 'without-functions 'lisp-indent-function
     (get 'let 'lisp-indent-function))

(defmacro without-user-input (&rest body)
  "Like `progn', but prevent any user interaction in BODY."
  `(without-functions (read-event)
     (without-minibuffer
       ,@body)))

(put 'without-user-input 'lisp-indent-function
     (get 'progn 'lisp-indent-function))

(provide 'without-user-input-settings)
