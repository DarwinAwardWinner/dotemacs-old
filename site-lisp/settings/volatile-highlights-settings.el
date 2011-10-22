(require 'volatile-highlights)
(require 'misc-settings)
(require 'cl)

;; M-x (customize-group 'volatile-highlights)

;; The following causes deletions to be highlighted, by highlighting
;; the character after the deletion.
(defun vhl/add-position (pos)
  "Highlight buffer position POS as a change. Just a position, not a range."
  (when (not (zerop (buffer-size)))
    (if (<= pos (buffer-size))
        (vhl/add pos (+ pos 1))
      (vhl/add (- pos 1) pos))))

(defadvice vhl/.make-vhl-on-change (after highlight-deletions activate)
  "Also highlight the position where something was deleted."
  (when (not (zerop len-removed))
    (vhl/add-position beg)))

;; The following makes it trivial to define simple vhl extensions
(defmacro vhl/define-extension (name &rest functions)
  "Define a VHL extension called NAME that applies standard VHL
  advice to each of FUNCTIONS."
  (assert (first functions))
  (let* ((name-string (symbol-name (eval name)))
         (function-list-string (make-list-string
                                (mapcar (lambda (f) (format "`%s'" (symbol-name (eval f))))
                                        functions)))
         (on-function-name (intern (format "vhl/ext/%s/on" name-string)))
         (on-body-form (cons
                        'progn
                        (mapcar (lambda (f)
                                  `(vhl/give-advice-to-make-vhl-on-changes ,(eval f)))
                                functions)))
         (on-doc-string (format "Turn on volatile highlighting for %s." function-list-string))

         (off-function-name (intern (format "vhl/ext/%s/off" name-string)))
         (off-body-form (cons
                         'progn
                         (mapcar (lambda (f)
                                   `(vhl/cancel-advice-to-make-vhl-on-changes ,(eval f)))
                                 functions)))
         (off-doc-string (format "Turn off volatile highlighting for %s." function-list-string)))
    `(progn
       (defun ,on-function-name ()
         ,on-doc-string
         (interactive)
         ,on-body-form)
       (defun ,off-function-name ()
         ,off-doc-string
         (interactive)
         ,off-body-form)
       nil)
))

;; Define an extension for killing
(vhl/define-extension 'kill 'kill-region)
(vhl/install-extension 'kill)
(vhl/load-extensions)
