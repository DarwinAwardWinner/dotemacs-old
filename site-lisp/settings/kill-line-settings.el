(defcustom kill-line-whitespace-blacklist-modes
  '(python-mode js2-mode)
  "Major modes in which kill-line should not attempt to fix up whitespace after killing."
:type '(repeat symbol))

;; ;; Kill-line likes whitespace now
;; (defadvice kill-line (after fixup-whitespace activate)
;;   "Call fixup-whitespace after killing line."
;;   (unless (or buffer-read-only
;;               (memq major-mode kill-line-whitespace-blacklist-modes))
;;     (save-excursion
;;       (fixup-whitespace)
;;       (when (and (fboundp indent-line-function) (looking-back "^\s*"))
;;         (funcall indent-line-function)))))
