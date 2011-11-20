(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.[mM][dD]\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.[mM][kK][dD][nN]\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.[mM][aA][rR][kK][dD][oO][wW][nN]\\'" . markdown-mode))

(defun markdown-linkify ()
  "Make region or current word into a link to itself."
  (interactive)
  (let* ((bounds
          (if (and mark-active transient-mark-mode)
             (cons (region-beginning) (region-end))
            (bounds-of-thing-at-point 'url)))
         (beg (car bounds))
         (end (cdr bounds))
         (url (buffer-substring beg end))
         (newtext (format "[%s](%s)" url url)))
    (delete-region beg end)
    (insert newtext)))

;; Trailing whitespace is significant in Markdown, so don't mess with it
(defadvice delete-trailing-whitespace (around disable-in-markdown activate)
  (unless (eq major-mode 'markdown-mode)
    ad-do-it))
