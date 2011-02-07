;; automatically marks files as executable if they begin with a valid #! line

;; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;(require 'shebang)
