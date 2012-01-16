;; TODO: Remove when this is upstreamed
(defun el-get-reinstall (package)
  "Remove PACKAGE and then install it again."
  (interactive (list (el-get-read-package-name "Reinstall")))
  (el-get-remove package)
  (el-get-install package))
