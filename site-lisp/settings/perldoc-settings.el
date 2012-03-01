;; (eval-after-load "perldoc"
;;   '(progn
;;      (defadvice perldoc-sentinel (after finishing-touches activate)
;;        "Really go to top of buffer, and mark it read-only."
;;        ;; If the perldoc buffer is current, then the command succeeded
;;        (cond ((string= (buffer-name (current-buffer))
;;                        "*Perldoc*")
;;               (goto-char (point-min))
;;               (toggle-read-only 1))))

;;      (defadvice perldoc-start-process (before make-buffer-rw activate)
;;        "Mark a read-only perldoc buffer read-write again before reusing."
;;        (with-current-buffer (get-buffer-create "*Perldoc*")
;;          (toggle-read-only 0)))))

(defalias 'perldoc 'cperl-perldoc)
