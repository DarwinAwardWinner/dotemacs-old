;; Must set some variables before loading el-get
(setq el-get-dir (file-name-as-directory
                  (expand-file-name "el-get" init-path))
      el-get-install-dir (concat el-get-dir "el-get")
      ;; Install el-get from *my* repo if it's not already installed.
      el-get-git-install-url "https://github.com/DarwinAwardWinner/el-get.git"
      el-get-git-url "https://github.com/DarwinAwardWinner/el-get.git"
      ;; Use the master branch of el-get
      el-get-master-branch t)

;; Ensure el-get is installed and set up
(when (file-directory-p el-get-install-dir)
  (add-to-list 'load-path el-get-install-dir))
(unless (require 'el-get nil 'noerror)
  (url-retrieve
   "https://github.com/DarwinAwardWinner/el-get/raw/custom-installer/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; Ensure package.el is installed and set up
(el-get 'sync 'package)

;; (when (not (string-equal (el-get-read-package-status "package") "installed"))
;;   (let ((current-prefix-arg t))
;;     (el-get-install "package")))
;; (el-get-init "package")
;; (require 'package)

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
(defvar el-get-sources-from-recipes
  '(anything
    apel
    auto-complete
    auto-install
    autopair
    bbdb
    browse-kill-ring
    clojure-mode
    dired+
    ;; emms
    full-ack
    highlight-parentheses
    magit
    magithub
    nav
    notify
    nxhtml
    org-mode
    package
    paredit
    smex
    undo-tree
    mode-compile
    pymacs
    python-mode
    ipython
    pylookup
    sml-modeline
    remember
    switch-window
    ))

;; Packages to install via elpa
(defvar el-get-sources-from-elpa
  '(
    ac-R
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
(defvar el-get-sources-from-emacswiki
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
(defvar el-get-sources-from-http
  '(
    "http://download.tuxfamily.org/user42/quick-yes.el"
    ;; "http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/ido.el"
    "http://homepage1.nifty.com/bmonkey/emacs/elisp/tmenu.el"
    ))

;; Look in ~/.emacs.d/el-get/el-get/recipes/ for known recipes
(setq
 el-get-sources
 (append
  ;; Don't compile el-get, because I've been modifying it so often.
  `((:name el-get
           :website "https://github.com/dimitri/el-get#readme"
           :description "Manage the external elisp bits and pieces you depend upon."
           :type git
           :branch "reload"
           :url ,el-get-git-install-url
           :features el-get
           :load    "el-get.el"
           :compile "el-get.el"))

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

  ;; This one is special because the name can't be auto-determined.
  '((:name ido-other-window
           :type git
           :url "git@gist.github.com:817266.git"))
  ;; Special because I need a specific branch
  '((:name ido-ubiquitous
           :type git
           :url "https://github.com/DarwinAwardWinner/ido-ubiquitous.git"
           :branch "master"))
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
 )
(loop for source in el-get-sources
      do (ignore-errors (el-get 'sync (list (el-get-source-name source)))))

(defun el-get-insert-recipe-name (recipe)
  "Prompt for the name of an existing recipe, then insert that name at point.

Mainly useful for the completion suggestions it provides."
  (interactive (list (el-get-read-recipe-name "Insert name of" 'required-match)))
  (insert recipe))
