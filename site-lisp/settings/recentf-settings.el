;; http://www.xsteve.at/prg/emacs/power-user-tips.html - recentf
;; Use M-x customize-group recentf
(require 'ido)
(require 'recentf)

(global-set-key (kbd "<f12>") 'recentf-open-files)

;; Bind M-F11 to a function that uses ido on the recently opened files
(defun xsteve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)

  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))

(global-set-key (kbd "<f11>") 'xsteve-ido-choose-from-recentf)
