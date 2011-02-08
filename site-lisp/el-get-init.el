;; Must set el-get-dir before loading el-get
(setq el-get-dir (file-name-as-directory
                  (expand-file-name "el-get" init-path))
      ;; Install el-get from *my* repo if it's not already installed.
      el-get-git-url "git@github.com:DarwinAwardWinner/el-get.git")

(let ((user-emacs-directory init-path))
  (if (or (require 'el-get nil 'noerror)
          (load (expand-file-name "el-get/el-get/el-get.el"
                                  user-emacs-directory)
                'noerror))
      t ;(message "el-get is already installed, try M-x el-get-update")
    (url-retrieve
     "https://github.com/DarwinAwardWinner/el-get/raw/master/el-get-install.el"
     (lambda (s)
       (end-of-buffer)
       (eval-print-last-sexp)))))

;; Install a version of package.el with multi-repo support first, so
;; it can be used to install others.
(when (not (string-equal (el-get-read-package-status "package24") "installed"))
  (let ((current-prefix-arg t))
    (el-get-install "package24")))

(el-get-init "package24" 'noerror)
(require 'package)

;; Not evaluated
'(
  (defcustom el-get-recipe-sources ()
    "List of packages with el-get recipes available"
    ;; TODO make it right
    )

  (defcustom el-get-recipe-sources ()
    "List of packages for el-get to install using elpa"
    ;; TODO make it right
    ;; Should be a list of cons cells with:
    ;; car = repo definition
    ;; cdr = list of pkgs from that repo
    :example '((("gnu" . "http://elpa.gnu.org/packages/")
                . (pkg1 pkg2))          ;These are from the gnu repo
               (("elpa" . "http://tromey.com/elpa/")
                . (pkg3 pkg4 pkg5))     ;These are from the elpa repo
               (("example-repo" . "http://example.com/packages/")
                . (pkg6 pkg7 pkg8)))
    )
  (defcustom el-get-apt-get-sources ()
    "List of packages for el-get to install using apt-get"
    ;; TODO make it right
    ;; Should just be a list of package names
    )

  ;; TODO Code to add the above to `el-get-sources'
  )

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

;; Look in ~/.emacs.d/el-get/el-get/recipes/ for known recipes
(progn
  (setq
   el-get-sources


   `(;; Don't compile el-get, because I've been modifying it so often.
     (:name el-get
            :compile nil
            :type git
            :url "git://github.com/DarwinAwardWinner/el-get.git")

     ;; Packages to install from existing recipes
     apel
     auto-complete
     auto-install
     autopair
     bbdb
     browse-kill-ring
     clojure-mode
     dired+
     full-ack
     highlight-parentheses
     magit
     nav
     notify
     nxhtml
     org-mode
     package24
     paredit
     smex
     undo-tree
     pymacs
     python-mode
     ipython
     pylookup
     sml-modeline
     remember
     switch-window

     ,@(mapcar
        (lambda (pkg)
          `(:name ,pkg
                  :type elpa))
        '(;; Packages to install via elpa
          c-eldoc
          clojure-test-mode
          compilation-recenter-end
          diminish
          dired-isearch
          iresize
          kill-ring-search
          parenface
          rainbow-mode
          smart-operator
          wtf
          ))

     ;; ,@(mapcar
     ;;    (lambda (pkg)
     ;;      `(:name ,pkg
     ;;              :type apt-get))
     ;;    '(;; Packages to install via apt
     ;;      sepia
     ;;      ))

     ,@(mapcar
        (lambda (pkg)
          `(:name ,pkg
                  :type emacswiki))
        '(;; Packages to install from emacswiki
          ac-R
          centered-cursor-mode
          csharp-mode
          cursor-chg
          highlight-cl
          icomplete+
          iedit
          mic-paren
          misc-cmds
          multi-term
          smart-compile+
          tempbuf
          winpoint
          yaoddmuse
          ))

     ,@(mapcar
        (lambda (url)
          `(:name ,(el-get-package-name-from-url url)
                  :type http
                  :url ,url))
        '(;; URL's of packages to install via http
          "http://download.tuxfamily.org/user42/quick-yes.el"
          "http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/ido.el"
          "http://homepage1.nifty.com/bmonkey/emacs/elisp/tmenu.el"
          ))

     (:name ido-other-window
            :type git
            :url "git@gist.github.com:817266.git")

     ,@(mapcar
        (lambda (url)
          `(:name ,(el-get-package-name-from-git url)
                  :type git
                  :url ,url))
        `(;; URL's of packages to install via git
          "git://github.com/k-talo/volatile-highlights.el.git"
          "git://git.naquadah.org/google-maps.git"
          "git://jblevins.org/git/markdown-mode.git"
          "git://github.com/jwiegley/regex-tool.git"
          "git@github.com:DarwinAwardWinner/cperl-mode.git"
          ,@(mapcar
             (lambda (name)
               (format "git://github.com/emacsmirror/%s.git" name))
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
  (el-get 'sync))

(defun el-get-insert-recipe-name (recipe)
  "Prompt for the name of an existing recipe, then insert that name at point.

Mainly useful for the completion suggestions it provides."
  (interactive (list (el-get-read-recipe-name "Insert name of" 'required-match)))
  (insert recipe))
