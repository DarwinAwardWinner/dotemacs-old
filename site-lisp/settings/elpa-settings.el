;;; ELPA - Emacs List Package Archive
;;; A tool for installing packages
;;; http://tromey.com/elpa/

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.

(when
    (load
     (expand-file-name "~/.emacs.d/site-lisp/packages/elpa/package.el")
     'noerror)
  (package-initialize))
