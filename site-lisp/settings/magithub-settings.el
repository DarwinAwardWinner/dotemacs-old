(require 'magithub)

;; This function tries to run in every buffer, and triggers errors on
;; TRAMP buffers whose remote is currently unavailable, since it tries
;; to check for git stuff on the remote. But it shoudn't do anything
;; for those buffers anyway, so just ignore such errors.
(defadvice magithub-try-enabling-minor-mode (around ignore-errors activate)
  (ignore-errors
    ad-do-it))

(defadvice magithub-parse-repo (after remove-spaces activate)
  "Remove leading and trailing spaces from parsed username and repo name"
  (let ((trim-space
         (apply-partially 'replace-regexp-in-string
                          "^ *\\(.*?\\) *$" "\\1")))
    (setq ad-return-value
          (cons (funcall trim-space (car ad-return-value))
                (funcall trim-space (cdr ad-return-value))))))
