(require 'defadvice-let)
(require 'org)
(require 'org-install)

;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; Must ensure font-lock on org-mode buffers
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; Turn off line wrapping
(add-hook 'org-mode-hook #'(lambda () (toggle-truncate-lines 1)))

;; Keep hierarchical TODOs consistent
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Dragging URLs

;; This function uses org-mode support for plain list to facilitate
;; dragging URLs from a webbrowser (or other apps) to an org-mode
;; buffer
(defadvice dnd-insert-text (around org-mouse-dnd-insert-text activate)
  (if (eq major-mode 'org-mode)
      (progn
	(cond
	 ;; if this is the end of the line then just insert text here
	 ((eolp)
	  (skip-chars-backward " \t")
	  (kill-region (point) (point-at-eol))
	  (unless (looking-back ":") (insert ":"))
	  (insert " "))

	 ;; if this is the beginning of the line then insert before
	 ((and (looking-at " \\|\t")
	       (save-excursion
		 (skip-chars-backward " \t") (bolp)))
	  (beginning-of-line)
	  (looking-at "[ \t]*")
	  (open-line 1)
	  (indent-to (- (match-end 0) (match-beginning 0)))
	  (insert "+ "))

	 ;; if this is a middle of the line, then insert after
	 (t
	  (end-of-line)
	  (newline t)
	  (indent-relative)
	  (insert "+ ")))
	(insert text)
	(beginning-of-line))
    ad-do-it))

(defun org-sort-multi (&rest sort-types)
  "Sort successively by a list of criteria.
For example, sort first by TODO status, then by priority, then by date, then alphabetically, case-sensitive.
Each criterion is either a character or a cons pair (BOOL . CHAR), where BOOL is whether or not to sort case-sensitively, and CHAR is one of the characters defined in ``org-sort-entries-or-items''.
So, the example above could be accomplished with:
 (org-sort-multi ?o ?p ?t (t . ?a))"
  (mapc #'(lambda (sort-type)
            (org-sort-entries-or-items
             (car-safe sort-type)
             (or (cdr-safe sort-type) sort-type)))
        (reverse sort-types)))

(defun org-sort-custom ()
  "Sort children of node by todo status and by priority, so the * TODO [#A] items go to the top. Then fold it the way I like it."
  (interactive)
  (org-sort-multi ?o ?p ?T)
  (dotimes (x 2) (org-cycle)))

(defcustom org-todo-keywords-sort-order nil
  "If you want your TODO keywords to sort in a different order
  than they cycle, set the cycle order in org-todo-keywords and
  the sort order here. Any keywords not listed here will sort
  after the ones listed here, in their normal order."
  :group 'org-todo
  :type '(repeat string))

(defadvice-let ((org-todo-keywords-1 (append org-todo-keywords-sort-order org-todo-keywords-1)))
  org-sort-entries-or-items
  "Allow TODO sort order to be different than TODO cycle order.")

;; (defadvice org-sort-entries-or-items (around correct-todo-sort-order activate)
;;   (let ((org-todo-keywords-1 (append org-todo-keywords-sort-order org-todo-keywords-1))) ad-do-it))

;; (defun org-completing-read-no-i (&rest args)
;;   (let (org-completion-use-ido org-completion-use-iswitchb)
;;     (apply 'org-completing-read args)))

(defun org-set-category (property value)
  "In the current entry, set category to VALUE.
When called interactively, this will prompt for a value, offering
completion either on allowed values (via an inherited xxx_ALL
property) or on existing values in other instances of this
property in the current file."
  (interactive
   (let* ((completion-ignore-case t)
	  (prop "CATEGORY")
	  (cur (org-entry-get nil prop))
	  (allowed (org-property-get-allowed-values nil prop 'table))
	  (existing (mapcar 'list (org-property-values prop)))
	  (val (if allowed
		   (org-completing-read "Value: " allowed nil 'req-match)
		 (let (org-completion-use-ido org-completion-use-iswitchb)
		   (org-completing-read
		    (concat "Value " (if (and cur (string-match "\\S-" cur))
					(concat "[" cur "]") "")
			    ": ")
		    existing nil nil "" nil cur)))))
     (list prop (if (equal val "") cur val))))
  (unless (equal (org-entry-get nil property) value)
    (org-entry-put nil property value)))

(defun org-delete-category ()
  "Delete the category of the current item."
  (interactive)
  (org-delete-property "CATEGORY"))

;; * Remove redundant tags of headlines
;; -- David Maus

;; A small function that processes all headlines in current buffer and
;; removes tags that are local to a headline and inherited by a parent
;; headline or the #+FILETAGS: statement.

(defun org-remove-redundant-tags-in-buffer ()
  "Remove redundant tags of headlines in current buffer.

  A tag is considered redundant if it is local to a headline and
  inherited by a parent headline."
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-map-entries
       '(lambda ()
          (let ((alltags (split-string (or (org-entry-get (point) "ALLTAGS") "") ":"))
                local inherited tag)
            (dolist (tag alltags)
              (if (get-text-property 0 'inherited tag)
                  (push tag inherited) (push tag local)))
            (dolist (tag local)
              (if (member tag inherited) (org-toggle-tag tag 'off)))))
       t nil))))
