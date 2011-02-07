(defadvice after-find-file (around prevent-change-buffer-modified-p activate)
  "Keep the value of `buffer-modified-p' unchanged.

When `require-final-newline' is set to `visit' or `visit-save',
the newline added in this function sets the modified flag on the
buffer. This is bad, because the user hasn't actually edited the
buffer yet. So this advice resets `buffer-modified-p' back to
whatever it was before the function started."
  (let ((bmodp (buffer-modified-p)))
    ad-do-it
    (set-buffer-modified-p bmodp)))
