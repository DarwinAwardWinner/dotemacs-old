;; (defun back-to-indentation-mark (&optional arg)
;;   "Ensure mark is active; move point to beginning of current line.
;; With argument ARG not nil or 1, move forward ARG - 1 lines first.
;; If scan reaches end of buffer, stop there without error."
;;   (interactive "p")
;;   (pc-select-ensure-mark)
;;   (beginning-of-line arg)
;;   (back-to-indentation))

;; (defun back-to-indentation-nomark (&optional arg)
;;   "Deactivate mark; move point to beginning of current line.
;; With argument ARG not nil or 1, move forward ARG - 1 lines first.
;; If scan reaches end of buffer, stop there without error."
;;   (interactive "p")
;;   (pc-select-maybe-deactivate-mark)
;;   (beginning-of-line arg)
;;   (back-to-indentation))

;; (defun pc-select-setup-remappings ()
;;   (set-default sym val)
;;   (mapc #'(lambda (pair)
;;             (mapc #'(lambda (marktype)
;;                       (define-key global-map
;;                         (vector 'remap (intern (concat (symbol-name (car pair)) "-" (symbol-name marktype))))
;;                         (intern (concat (symbol-name (cdr pair)) "-" (symbol-name marktype)))))
;;                   '(mark nomark)))
;;         pc-select-remapping-alist))

;; (defcustom pc-select-remapping-alist
;;   '((beginning-of-line . back-to-indentation))
;;   "Alist of command-remappings to do.
;; Leave out the ``-mark'' or ``-nomark'', because these will both be automatically added. For example, remapping ``beginning-of-line'' to ``back-to-indentation'' will actually replace both ``beginning-of-line-mark'' and ``beginning-of-line-nomark'' with the corresponding ``back-to-indentation'' functions."
;;   :type '(alist :key-type symbol :value-type symbol)
;;   :group 'pc-select
;;   :set #'(lambda (sym val) (set-default sym val) (pc-select-setup-remappings)))

;; (add-hook 'pc-selection-mode-hook
;;           'pc-select-setup-remappings)