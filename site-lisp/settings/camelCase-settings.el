(defun backward-hyphenated-or-underscore-word ()
  (interactive)
  (backward-word)
  (while (looking-back "[-_]") (backward-word)))

(defun camelCase-previous-word ()
  "Convert the previous word (including hyphens and underscores) to camelCase."
  (interactive)
  (let ((case-fold-search nil)
        (bound (point)))
    (save-excursion
      (backward-hyphenated-or-underscore-word)
      (while (re-search-forward "[\-_]\\([a-zA-Z]\\)" bound t)
        (replace-match (upcase (match-string 1)) nil 'literal)))))

(defun unCamelCase-previous-word (&optional sep)
  "If previous word is camelCased, convert it to a word separated by SEP.

Default separator is underscore."
  (interactive)
  (let ((case-fold-search nil)
        (bound (point))
        (sep (or sep "_")))
    (save-excursion
      (backward-hyphenated-or-underscore-word)
      (while (re-search-forward "\\([a-z]\\)\\([A-Z]\\)" bound t)
        (replace-match (concat (match-string 1) sep (downcase (match-string 2))) nil 'literal)))))
