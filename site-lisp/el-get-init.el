;; Must set some variables before loading el-get
(setq el-get-dir (file-name-as-directory
                  (expand-file-name "el-get" init-path))
      el-get-install-dir (concat el-get-dir "el-get")
      ;; Install el-get from *my* repo if it's not already installed.
      el-get-git-url "https://github.com/DarwinAwardWinner/el-get.git"
      el-get-git-install-url el-get-git-url
      ;; Use the master branch of el-get
      el-get-install-branch "master")

;; Eval this block to set up `el-get-sources'
(progn
  ;; Helper functions
  (defun el-get-package-name-from-url (url)
    (let ((basename (file-name-nondirectory url)))
      (if (string-match-p "\\.el$" basename)
	  (file-name-sans-extension basename)
	basename)))

  (defun el-get-package-name-from-git (url)
    (let ((basename (file-name-nondirectory url)))
      (if (string-match "^\\(.*?\\)\\(\\.el\\)?\\(\\.git\\)?$" basename)
	  (match-string 1 basename)
	url)))
  ;; Packages to install from existing recipes
  (setq el-get-sources-from-recipes
        '(anything
          apel
          auto-complete
          auto-install
          autopair
          bbdb
          browse-kill-ring
          clojure-mode
          dired+
          doc-mode
          emms
          ess
          highlight-parentheses
          (:name magit
                 :url "https://github.com/DarwinAwardWinner/magit.git"
                 :branch "what-im-running")
          (:name magithub
                 :username "DarwinAwardWinner"
                 :url "https://github.com/DarwinAwardWinner/magithub.git"
                 :branch "improvements")
          nav
          nxhtml
          org-mode
          package
          paredit
          (:name smex
                 :url "https://github.com/DarwinAwardWinner/smex.git"
                 :branch "master")
          undo-tree
          (:name mode-compile
                 :before (progn
                           (when (fboundp 'define-obsolete-variable-alias)
                             (provide 'obsolete))))
          python-mode
          pymacs
          ipython
          pylookup
          sml-modeline
          remember
          revive-plus
          js2-mode
          wanderlust
          ))

  ;; Packages to install via elpa
  (setq el-get-sources-from-elpa
        '(
          c-eldoc
          compilation-recenter-end
          diminish
          dired-isearch
          iresize
          kill-ring-search
          parenface
          rainbow-mode
          rainbow-delimiters
          marmalade
          furl
          smart-operator
          wtf
          yaml-mode
          ido-yes-or-no
          icomplete+
          multi-term
          yaoddmuse
          ))

  ;; Packages to install from emacswiki
  (setq el-get-sources-from-emacswiki
        '(
          centered-cursor-mode
          csharp-mode
          cursor-chg
          highlight-cl
          iedit
          mic-paren
          misc-cmds
          smart-compile+
          tempbuf
          winpoint
          ))

  ;; URL's of packages to install via http
  (setq el-get-sources-from-http
        '(
          "http://download.tuxfamily.org/user42/quick-yes.el"
          ;; "http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/ido.el"
          "http://homepage1.nifty.com/bmonkey/emacs/elisp/tmenu.el"
          "https://github.com/myuhe/auto-complete-acr.el/raw/master/auto-complete-acr.el"
          ))

  ;; Look in ~/.emacs.d/el-get/el-get/recipes/ for known recipes
  (setq
   el-get-sources
   (append
    ;; My own el-get
    `((:name el-get
             :website "https://github.com/dimitri/el-get#readme"
             :description "Manage the external elisp bits and pieces you depend upon."
             :type git
             :branch ,el-get-install-branch
             :url ,el-get-git-install-url
             :features el-get
             :load    "el-get.el"
             :compile ".")
      (:name zeitgeist
             :type http
             :url "http://bazaar.launchpad.net/~zeitgeist-dataproviders/zeitgeist-datasources/trunk/download/head:/zeitgeist.el-20100805191722-cqiaypshgst5y0we-3/zeitgeist.el"
             :features zeitgeist))

    el-get-sources-from-recipes

    (mapcar (lambda (pkg) `(:name ,pkg :type elpa))
            el-get-sources-from-elpa)

    (mapcar (lambda (pkg) `(:name ,pkg :type emacswiki))
            el-get-sources-from-emacswiki)

    (mapcar (lambda (url)
              `(:name ,(el-get-package-name-from-url url)
                      :type http
                      :url ,url))
            el-get-sources-from-http)

    ;; For testing purposes
    '((:name fake-test-pkg
             :type builtin))

    ;; This one is special because the name can't be auto-determined.
    '((:name ido-other-window
             :type git
             :url "git://gist.github.com/817266.git"))
    ;; Special because I need a specific branch
    '((:name ido-ubiquitous
             :type git
             :url "https://github.com/DarwinAwardWinner/ido-ubiquitous.git"
             :branch "master"))
    ;; Special because I need to add specific directories to load path
    '((:name git-wip
             :type git
             :url "https://github.com/DarwinAwardWinner/git-wip"
             :load-path ("emacs")))
    `(
      ,@(mapcar
         (lambda (url)
           `(:name ,(el-get-package-name-from-git url)
                   :type git
                   :url ,url))
         `(;; URL's of packages to install via git
           "git://git.naquadah.org/google-maps.git"
           "git://jblevins.org/git/markdown-mode.git"
           "https://github.com/k-talo/volatile-highlights.el.git"
           "https://github.com/jwiegley/regex-tool.git"
           "https://github.com/jrockway/cperl-mode.git"
           "https://github.com/juster/Sepia.git"
           "https://github.com/jhelwig/ack-and-a-half.git"
           "https://github.com/DarwinAwardWinner/git-commit-mode"
           "https://github.com/magnars/expand-region.el.git"
           ,@(mapcar
              (lambda (name)
                (format "https://github.com/emacsmirror/%s.git" name))
              '(;; URL's of packages on emacsmirror
                read-library
                offlineimap
                keydef
                tooltip-help
                smart-dash
                smart-mark
                sackspace
                query
                pointback
                multi-eshell
                eval-sexp-in-comments
                ))
           ))
      ))
   ))

;; Ensure el-get is installed and set up
(when (file-directory-p el-get-install-dir)
  (add-to-list 'load-path el-get-install-dir))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/DarwinAwardWinner/el-get/raw/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

;; Ensure el-get was installed successfully by the above
(require 'el-get)
;; Ensure package.el is installed and set up
(el-get 'sync 'package)

;; (when (not (string-equal (el-get-read-package-status "package") "installed"))
;;   (let ((current-prefix-arg t))
;;     (el-get-install "package")))
;; (el-get-init "package")
;; (require 'package)

(loop for source in el-get-sources
      do (ignore-errors (el-get 'sync (list (el-get-source-name source)))))

(defun el-get-insert-recipe-name (recipe)
  "Prompt for the name of an existing recipe, then insert that name at point.

Mainly useful for the completion suggestions it provides."
  (interactive (list (el-get-read-recipe-name "Insert name of" 'required-match)))
  (insert recipe))
