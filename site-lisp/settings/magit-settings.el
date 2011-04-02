(require 'magit)
(global-set-key (kbd "C-c C-m") 'magit-status)

(defadvice magit-run* (around use-myinit activate)
  "use git-myinit instead of git-init"
  (when (equal (ad-get-arg 0) (list "git" "init"))
    (ad-set-arg 0 (list "git" "myinit")))
  ad-do-it)


(defface magit-log-head-label-wip
  '((((class color) (background light))
     :box t
     :background "Grey95"
     :foreground "LightSkyBlue3")
    (((class color) (background dark))
     :box t
     :background "Grey07"
     :foreground "LightSkyBlue4"))
  "Face for local branch head labels shown in log buffer."
  :group 'magit)

(setq magit-present-log-line-function 'magit-present-log-line-custom)
(defun magit-present-log-line-custom (graph sha1 refs message)
  "My custom log line generator with support for git-wip."
  (let* ((ref-re "\\(?:tag: \\)?refs/\\(bisect\\|tags\\|remotes\\|patches/[^/]*\\|heads\\|wip\\)/\\(.+\\)")
	 (string-refs
	  (when refs
            (message "REFS: %S" refs)
	    (concat (mapconcat
		     (lambda (r)
		       (propertize
			(if (string-match ref-re r)
                            (concat
                             (when (member* (match-string 1 r) '("wip")
                                            :test 'string=)
                               (concat (match-string 1 r) "/"))
                             (match-string 2 r))
			  r)
			'face (cond
			       ((string= r "refs/stash")
				'magit-log-head-label-local)
			       ((string= (match-string 1 r) "remotes")
				'magit-log-head-label-remote)
			       ((magit-string-match-p "^patches/[^/]*$" (or (match-string 1 r) "")) ; Stacked Git
				'magit-log-head-label-patches)
			       ((string= (match-string 1 r) "bisect")
				(if (string= (match-string 2 r) "bad")
				    'magit-log-head-label-bisect-bad
				  'magit-log-head-label-bisect-good))
			       ((string= (match-string 1 r) "tags")
				'magit-log-head-label-tags)
			       ((string= (match-string 1 r) "heads")
				'magit-log-head-label-local)
                               ((string= (match-string 1 r) "wip")
                                'magit-log-head-label-wip))))
		     refs
		     " ")
		    " "))))
    (concat
     (if sha1
	 (propertize (substring sha1 0 8) 'face 'magit-log-sha1)
       (insert-char ? 8))
     " "
     (when graph
       (propertize graph 'face 'magit-log-graph))
     string-refs
     (when message
       (propertize message 'face 'magit-log-message)))))
