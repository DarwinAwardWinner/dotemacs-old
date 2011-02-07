;; (eval-after-load 'filladapt
;;   '(progn
;;      (defun filladapt-fill-paragraph (function &optional justify region)
;;        (catch 'done
;;          (if (and filladapt-mode (null fill-prefix))
;;              (save-restriction
;;                (let ((paragraph-ignore-fill-prefix nil)
;;                      ;; if the user wanted this stuff, they probably
;;                      ;; wouldn't be using filladapt-mode.
;;                      (adaptive-fill-mode nil)
;;                      (adaptive-fill-regexp nil)
;;                      ;; need this or Emacs 19 ignores fill-prefix when
;;                      ;; inside a comment.
;;                      (comment-multi-line t)
;;                      fill-prefix retval)
;;                  (if (filladapt-adapt t nil)
;;                      (progn
;;                        (if filladapt-fill-column-tolerance
;;                            (let* ((low (- fill-column
;;                                           filladapt-fill-column-backward-fuzz))
;;                                   (high (+ fill-column
;;                                            filladapt-fill-column-forward-fuzz))
;;                                   (old-fill-column fill-column)
;;                                   (fill-column fill-column)
;;                                   (lim (- high low))
;;                                   (done nil)
;;                                   (sign 1)
;;                                   (delta 0))
;;                              (while (not done)
;;                                (setq retval (filladapt-funcall function justify region))
;;                                (if (filladapt-paragraph-within-fill-tolerance)
;;                                    (setq done 'success)
;;                                  (setq delta (1+ delta)
;;                                        sign (* sign -1)
;;                                        fill-column (+ fill-column (* delta sign)))
;;                                  (while (and (<= delta lim)
;;                                              (or (< fill-column low)
;;                                                  (> fill-column high)))
;;                                    (setq delta (1+ delta)
;;                                          sign (* sign -1)
;;                                          fill-column (+ fill-column
;;                                                         (* delta sign))))
;;                                  (setq done (> delta lim))))
;;                              ;; if the paragraph lines never fell
;;                              ;; within the tolerances, refill using
;;                              ;; the old fill-column.
;;                              (if (not (eq done 'success))
;;                                  (let ((fill-column old-fill-column))
;;                                    (setq retval (filladapt-funcall function justify region)))))
;;                          (setq retval (filladapt-funcall function justify region)))
;;                        (run-hooks 'filladapt-fill-paragraph-post-hook)
;;                        (throw 'done retval))))))
;;          ;; filladapt-adapt failed, so do fill-paragraph normally.
;;          (filladapt-funcall function justify region)))

;;      (defun fill-paragraph (&optional justify region)
;;        "Fill paragraph at or after point.  Prefix arg means justify as well.
;; \(This function has been overloaded with the `filladapt' version.)

;; If `sentence-end-double-space' is non-nil, then period followed by one
;; space does not end a sentence, so don't break a line there.

;; If `fill-paragraph-function' is non-nil, we call it (passing our
;; argument to it), and if it returns non-nil, we simply return its value."
;;        (interactive "*P")
;;        (let ((filladapt-inside-filladapt t))
;;          (filladapt-fill-paragraph 'fill-paragraph justify region)))

;;      (defadvice fill-paragraph (around nothing activate)
;;        ad-do-it)

;; ))
