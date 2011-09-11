(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))

;; Any *.conf in these directories will be opened in apache-mode
(setq apache-conf-paths '("/etc/apache2"))

(defun abspathp (path)
  (string= "/" (substring path 0 1)))

(let ((apache-conf-regexp
       (concat
        ;; Open group
        "\\(?:"
        ;; Any directory in apache-conf-paths
        (mapconcat (lambda (path)
                     (concat (when (abspathp path)
                               "^")
                             path
                             (unless (string= "/" (substring path -1))
                               "/")))
                   apache-conf-paths
                   "\\|")
        ;; Close group
        "\\)"
        ;; Anything else
        ".+"
        ;; Ends in ".conf"
        "\\.conf$")))
  (add-to-list 'auto-mode-alist (cons apache-conf-regexp 'apache-mode)))

