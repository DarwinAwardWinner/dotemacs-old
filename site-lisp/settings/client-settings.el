(defvar sacrificial-frame nil
  "This variable holds a decoy frame in order to work around the odd behavior of emacsclent.")

(defmacro with-invisible-background-frame (&rest body)
  "Like `progn', but if emacs is running as a deamon, this
  evaluates the body after creating a new frame on the current X
  display and making it invisible. Also, any errors are ignored
  in the body, because I can't fathom emacs' error-handling
  functions. This function exists to work around one of the
  quirks of emacsclient."
  (if (daemonp)
      `(progn
         ;; Delete previous sacrificial frame and make a new one
         (when sacrificial-frame
           (delete-frame sacrificial-frame t))
         (setq sacrificial-frame (make-frame))
         (make-frame-invisible sacrificial-frame t)
         ,@body)
    `(progn ,@body)))
