(defvar perl-test-file-regexp
  "/x?t/.+\.t$"
  "Regular expression that should match perl distribution globtest files.")

(defun perl-test-file-p (&optional buf)
  "Returns t if BUF looks like a perl test file.

If BUF is nil, check the current buffer."
  (with-current-buffer (or buf (current-buffer))
    (let ((path (buffer-file-name)))
      (and
       ;; Path is defined
       path
       ;; Path looks like a test file
       (string-match-p "/t/.+\.t$" path)
       ;; Empty or starts with #!perl
       (or
        (= (buffer-size) 0)
        (save-excursion
          (goto-char (point-min))
          (looking-at-p "#![^\n]*perl"))
        (save-excursion
          (goto-char (point-min))
          (search-forward-regexp (concat "^[[:space:]]*" (regexp-quote "use Test::")) nil 'noerror)))))))

(defcustom perl-test-file-boilerplate
  "#!perl"
  "Boilerplate text to be added to the beginning of a new perl test file.

A trailing newline will always be added, so you can leave it off."
  :type '(string :tag "Template"))

(defun perl-test-file-do-setup (&optional buf)
  "Set up a new perl test file to be font-locked as perl.

Simply insert `perl-test-file-boilerplate' if the file is empty,
then call `normal-mode' to set up. Mostly, just add #!perl to the
beginning so that it gets recognized as a perl-mode file, and
call `perl-mode' to set the major mode."

  ;; Recursion protect
  (when (not (boundp 'perl-test-file-already-setup))
    (let (perl-test-file-already-setup)
      ;; Insert boilerplate if text is empty
      (when (= (buffer-size) 0)
        ;; Boilerplate does not count as modification
        (let ((previous-buffer-modified-state (buffer-modified-p)))
          (insert perl-test-file-boilerplate)
          (insert "\n")
          (set-buffer-modified-p previous-buffer-modified-state)))

      (perl-mode))))

(add-to-list 'magic-fallback-mode-alist
             '(perl-test-file-p . perl-test-file-do-setup))
