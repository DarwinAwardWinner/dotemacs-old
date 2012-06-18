;; Must set some variables before loading el-get
(setq el-get-dir (file-name-as-directory
                  (expand-file-name "el-get" init-path))
      el-get-install-dir (concat el-get-dir "el-get")
      ;; Install el-get from *my* repo if it's not already installed.
      el-get-git-url "https://github.com/DarwinAwardWinner/el-get.git"
      el-get-git-install-url el-get-git-url
      ;; Use the master branch of el-get
      el-get-install-branch "master"
      el-get-verbose t
      el-get-default-process-sync t)

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
          rect-mark
          smart-compile+
          tempbuf
          winpoint
          yaoddmuse
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
             :compile "."))

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

    ;; Can't use the builtin recipes because cvs is awful
    '((:name apel
            :website "http://www.kanji.zinbun.kyoto-u.ac.jp/~tomo/elisp/APEL/"
            :description "APEL (A Portable Emacs Library) is a library to support to write portable Emacs Lisp programs."
            :type http-tar
            :module "apel"
	    :url "http://archive.ubuntu.com/ubuntu/pool/universe/a/apel/apel_10.8.orig.tar.gz"
            :options ("xzf")
            :build
            (mapcar
             (lambda (target)
               (list el-get-emacs
                     (split-string "-batch -q -no-site-file -l APEL-MK -f")
                     target
                     "prefix" "site-lisp" "site-lisp"))
             '("compile-apel" "install-apel"))
            :load-path ("site-lisp/apel" "site-lisp/emu"))
      (:name flim
             :description "A library to provide basic features about message representation or encoding"
             :depends apel
             :type http-tar
             :url "http://archive.ubuntu.com/ubuntu/pool/universe/f/flim/flim_1.14.9+0.20110516.orig.tar.gz"
             :options ("xzf")
             ;; :type cvs
             ;; :module "flim"
             ;; :url ":pserver:anonymous@cvs.m17n.org:/cvs/root"
             :build
             (mapcar
              (lambda (target)
                (list el-get-emacs
                      (mapcar (lambda (pkg)
                                (mapcar (lambda (d) `("-L" ,d)) (el-get-load-path pkg)))
                              '("apel"))
                      (split-string "-batch -q -no-site-file -l FLIM-MK -f")
                      target
                      "prefix" "site-lisp" "site-lisp"))
              '("compile-flim" "install-flim"))
             :load-path ("site-lisp/flim"))
      (:name semi
             :description "SEMI is a library to provide MIME feature for GNU Emacs."
             :depends flim
             :type http-tar
             :url "http://archive.ubuntu.com/ubuntu/pool/universe/s/semi/semi_1.14.6+0.20101114.orig.tar.gz"
             :options ("xzf")
             ;; :type cvs
             ;; :module "semi"
             ;; :url ":pserver:anonymous@cvs.m17n.org:/cvs/root"
             :build
             (mapcar
              (lambda (target)
                (list el-get-emacs
                      (mapcar (lambda (pkg)
                                (mapcar (lambda (d) `("-L" ,d)) (el-get-load-path pkg)))
                              '("apel" "flim"))
                      (split-string "-batch -q -no-site-file -l SEMI-MK -f")
                      target
                      "prefix" "site-lisp" "site-lisp"))
              '("compile-semi" "install-semi"))
             :load-path ("site-lisp/semi/"))
      (:name wanderlust
       :description "Wanderlust bootstrap."
       :depends semi
       :type http-tar
       ;; :url "file:///home/ryan/temp/wl_2.14.0.orig.tar.gz"
       :url "http://archive.ubuntu.com/ubuntu/pool/universe/w/wl/wl_2.14.0.orig.tar.gz"
       :options ("xzf")
       ;; :type cvs
       ;; :module "wanderlust"
       ;; :url ":pserver:anonymous@cvs.m17n.org:/cvs/root"
       :build
       (mapcar
        (lambda (target-and-dirs)
          (list el-get-emacs
                (mapcar (lambda (pkg)
                          (mapcar (lambda (d) `("-L" ,d)) (el-get-load-path pkg)))
                        (append
                         '("apel" "flim" "semi")
                         (when (el-get-package-exists-p "bbdb") (list "bbdb"))))
		;; Force the coding system to utf-8 so that Emacs
		;; doesn't try to prompt for it so it can run
		;; noninteractively.
		"--eval" (el-get-print-to-string
			  '(defun select-safe-coding-system (&rest ignored) (quote utf-8)))
                "--eval" (el-get-print-to-string
                          '(progn (setq wl-install-utils t)
                                  (setq wl-info-lang "en")
                                  (setq wl-news-lang "en")))

                (split-string "-batch -q -no-site-file -l WL-MK -f")
                target-and-dirs))
        '(("compile-wl-package"  "site-lisp" "icons")
          ("install-wl-package" "site-lisp" "icons")
	  ("wl-texinfo-format" "doc")))
       :info "doc/wl.info"
       :load-path ("site-lisp/wl" "utils"))
      (:name emacs-goodies-el
             :website "http://packages.debian.org/sid/emacs-goodies-el"
             :description "Miscellaneous add-ons for Emacs"
             :type http-tar
             :url "http://alioth.debian.org/snapshots.php?group_id=30060"
             :options ("xzf")
             :build
             (let ((makerfiles
                    (split-string (shell-command-to-string "find . -name '*.make'"))))
               (mapcar
                (lambda (makerfile)
                  (let ((maker-dir (file-name-directory makerfile))
                        (maker-command
                         (replace-regexp-in-string
                          "\n" ""
                          (replace-regexp-in-string
                           "^emacs" el-get-emacs
                           (with-temp-buffer
                             (insert-file-contents makerfile)
                             (buffer-string))))))
                    (format "cd %s && %s" maker-dir maker-command)))
                makerfiles))
             :load-path ("emacs-goodies-el/elisp/debian-el"
                         "emacs-goodies-el/elisp/devscripts-el"
                         "emacs-goodies-el/elisp/dpkg-dev-el"
                         "emacs-goodies-el/elisp/emacs-goodies-el"
                         ;; "emacs-goodies-el/elisp/gnus-bonus-el"
                         "emacs-goodies-el/elisp/vm-bonus-el")
             ;; The :features and :post-init are taken from the
             ;; debain/*.emacsen-startup files in the source package.
             ;; Some of these files just require specific features,
             ;; and those are added to :features. Others have some
             ;; significant init code, and this has been manually
             ;; copied into :post-init. The emacsen-startup files
             ;; cannot be used directly because they have hardcoded
             ;; paths to where dpkg would install the packages.
             :features (emacs-goodies-el
                        debian-el
                        dpkg-dev-el)
             :post-init
             (progn
                          ;; autoloads for devscripts.el
                          (autoload 'debuild "devscripts" "Run debuild in the current directory." t)
                          (autoload 'debc "devscripts" "Run debc in the current directory." t)
                          (autoload 'debi "devscripts" "Run debi in the current directory." t)
                          (autoload 'debit "devscripts" "Run debit in the current directory." t)
                          (autoload 'debdiff "devscripts" "Compare contents of CHANGES-FILE-1 and CHANGES-FILE-2." t)
                          (autoload 'debdiff-current "devscripts"   "Compare the contents of .changes file of current version with previous version;
requires access to debian/changelog, and being in debian/ dir." t)
                          (autoload 'debclean "devscripts" "Run debclean in the current directory." t)
                          (autoload 'pdebuild "pbuilder-mode" "Run pdebuild in the current directory." t)
                          (autoload 'pdebuild-user-mode-linux "pbuilder-mode" "Run pdebuild-user-mode-linux in the current directory." t)
                          (autoload 'pbuilder-log-view-elserv "pbuilder-log-view-mode" "Run a elserv session with log view.

Running this requires elserv.  Use elserv, and do `elserv-start' before invoking this command." t)
                          (autoload 'debuild-pbuilder "pbuilder-mode" "Run debuild-pbuilder in the current directory." t)
                          (autoload 'pbuilder-build "pbuilder-mode" "Run pbuilder-build for the given filename." t)
                          (autoload 'pbuilder-user-mode-linux-build "pbuilder-mode" "Run pbuilder-user-mode-linux for the given filename." t)

                          ;; from "debian/vm-bonus-el.emacsen-startup"
                          (defgroup vm-bonus-el nil
                            "Customize vm-bonus-el Debian packages."
                            :group 'vm)

                          ;; vm-bogofilter.el
                          (defgroup vm-bogofilter nil
                            "VM Spam Filter Options"
                            :group 'vm
                            :group 'vm-bonus-el
                            :load 'vm-bogofilter)

                          (autoload 'vm-bogofilter-setup "vm-bogofilter"
                            "Initialize vm-bogofilter."
                            t)

                          (defcustom vm-bogofilter-setup nil
                            "Whether to initialize vm-bogofilter on startup.
vm-bogofilter interfaces VM with the bogofilter spam filter."
                            :type 'boolean
                            :set (lambda (symbol value)
                                   (set-default symbol value)
                                   (when value
                                     (vm-bogofilter-setup)))
                            :load 'vm-bogofilter
                            :group 'vm
                            :group 'vm-bogofilter
                            :group 'vm-bonus-el))))


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
                eval-sexp-in-comments))))))))

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

;; Ensure package.el is installed and set up. This handles switching
;; from Emacs 24 to Emacs 23, where pacakge.el is suddenly not
;; available and needs to be installed as a real package. A manual
;; reinstall from Emacs 24 would bring it back to being just a dummy.
(message "Setting up package.el")
(ignore-errors (el-get 'sync 'package))
(unless (require 'package nil 'noerror)
  (message "Need to reinstall package.el")
  (let ((el-get-default-process-sync t))
    (el-get-reinstall 'package)
    (el-get 'sync 'package)
    (require 'package)))
(message "Package.el set up successfully")

(defadvice el-get-start-process-list (around allow-deep-recursion activate)
  ;; Add 100 to binding depth limit each time, allowing
  ;; el-get-start-process-list to recurse arbitrarily deep.
  (let ((max-specpdl-size (+ max-specpdl-size 100)))
    ad-do-it))

(loop for source in el-get-sources
      for pkg = (el-get-source-name source)
      do (condition-case e
             (el-get 'sync pkg)
           (error (warn "Error while installing package %s via el-get: %S" pkg (cdr e)))))

(defun el-get-insert-recipe-name (recipe)
  "Prompt for the name of an existing recipe, then insert that name at point.

Mainly useful for the completion suggestions it provides."
  (interactive (list (el-get-read-recipe-name "Insert name of" 'required-match)))
  (insert recipe))
