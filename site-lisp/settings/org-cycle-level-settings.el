;; This file implements my own custom org-cycle-level

(require 'cl)
(require 'org)

(defun org-point-at-end-of-empty-headline ()
  "If point is at the end of an empty headline, return t, else nil."
  (and (looking-at "[ \t]*$")
       (save-excursion
         (beginning-of-line 1)
         (looking-at (concat "^\\(\\*+\\)[ \t]+\\(" org-todo-regexp "\\)?[ \t]*$")))))

(defun org-level-increment ()
  "Return the number of stars that will be added or removed at a
time to headlines when structure editing, based on the value of
`org-odd-levels-only'."
  (if org-odd-levels-only 2 1))

(defvar org-previous-line-level-cached nil)

(defun org-recalculate-previous-line-level ()
  "Same as `org-get-previous-line-level', but does not use cached
value. It does *set* the cached value, though."
  (set 'org-previous-line-level-cached
       (let ((current-level (org-current-level))
             (prev-level (when (> (line-number-at-pos) 1)
                           (save-excursion
                             (previous-line)
                             (org-current-level)))))
         (cond ((null current-level) nil) ; Before first headline
               ((null prev-level) 0)      ; At first headline
               (prev-level)))))

(defun org-get-previous-line-level ()
  "Return the outline depth of the last headline before the
current line. Returns 0 for the first headline in the buffer, and
nil if before the first headline."
  ;; This calculation is quite expensive, with all the regex searching
  ;; and stuff. Since org-cycle-level won't change lines, we can reuse
  ;; the last value of this command.
  (or (and (eq last-command 'org-cycle-level)
           org-previous-line-level-cached)
      (org-recalculate-previous-line-level)))

(defun org-cycle-level ()
  (interactive)
  (let ((org-adapt-indentation nil))
    (when (org-point-at-end-of-empty-headline)
      (setq this-command 'org-cycle-level) ;Only needed for caching
      (let ((cur-level (org-current-level))
            (prev-level (org-get-previous-line-level)))
        (cond
         ;; If first headline in file, promote to top-level.
         ((= prev-level 0)
          (loop repeat (/ (- cur-level 1) (org-level-increment))
                do (org-do-promote)))
         ;; If same level as prev, demote one.
         ((= prev-level cur-level)
          (org-do-demote))
         ;; If parent is top-level, promote to top level if not already.
         ((= prev-level 1)
          (loop repeat (/ (- cur-level 1) (org-level-increment))
                do (org-do-promote)))
         ;; If top-level, return to prev-level.
         ((= cur-level 1)
          (loop repeat (/ (- prev-level 1) (org-level-increment))
                do (org-do-demote)))
         ;; If less than prev-level, promote one.
         ((< cur-level prev-level)
          (org-do-promote))
         ;; If deeper than prev-level, promote until higher than
         ;; prev-level.
         ((> cur-level prev-level)
          (loop repeat (+ 1 (/ (- cur-level prev-level) (org-level-increment)))
                do (org-do-promote))))
        t))))
