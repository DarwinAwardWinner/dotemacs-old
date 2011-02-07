(require 'elscreen)

(defvar elscreen-modules
  '(dired
    dnd
    ;server
    buffer-list)
  "List of elscreen files to load")

(mapc #'(lambda (module) (require (intern (concat "elscreen-" (symbol-name module))))) elscreen-modules)

;; Make C-z C-z toggle screens
(define-key elscreen-map (kbd "C-z") 'elscreen-toggle)

