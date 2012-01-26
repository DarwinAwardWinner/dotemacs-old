(require 'python-mode)

(define-key py-mode-map (kbd "C-c C-m") nil)
(define-key py-mode-map (kbd "C-<backspace>") nil)

(defun py-comment-insert-comment-function ()
  """If point matches indentation of next line, don't change it."
  (let (comment-insert-comment-function ;Prevent recursion
        (current-indent (- (point) (line-beginning-position)))
        (prev-indent (save-excursion
                       (py-beginning-of-statement)
                       (current-indentation)))
        (next-indent (save-excursion
                       (py-goto-statement-below)
                       (current-indentation))))
    (cond ((= current-indent next-indent)
           ;; Point matches next indentation, so don't change it
           (insert "# "))
          ;; Anything else
          (t (call-interactively 'comment-dwim)))))

(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'comment-insert-comment-function)
                 'py-comment-insert-comment-function)))

;; Make sure we clean up when switching out of python mode
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (and (eq comment-insert-comment-function 'py-comment-insert-comment-function)
                       (not (eq major-mode 'python-mode)))
              (set (make-local-variable 'comment-insert-comment-function) nil))))

(defadvice py-indent-line (around save-excursion activate)
  "Prevent [TAB] from moving point to the beginning of the line.

This makes Python's indentation behavior consistent with all
other modes."
  (if (<= (current-column) (current-indentation))
      (progn
        (move-to-column (current-indentation))
        ad-do-it)
    (save-excursion
      (move-to-column (current-indentation))
      ad-do-it)))

(defcustom py-electric-colon-dedent-only nil
  "Only allow py-electric-colon to dedent a line, not indent it further."
  :type 'boolean)

(defadvice py-electric-colon (around dedent-only activate)
  "Only allow py-electric-colon to dedent a line, not indent it further."
  (if py-electric-colon-dedent-only
      (let ((indentation-before (current-indentation)))
        ad-do-it
        (when (> (current-indentation) indentation-before)
          (save-excursion
            (beginning-of-line)
            (delete-forward-char (- (current-indentation) indentation-before)))))
    ad-do-it))

;; This completely overrides the existing function
(defadvice py-newline-and-indent (around fix activate)
  "Add a newline and indent to outmost reasonable indent.
When indent is set back manually, this is honoured in following lines.

This modified version always ends with point at end of
indentation, and never indents an existing line further than the
current indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        erg)
    (if (< ci (current-column))         ; if point beyond indentation
        (progn
          (newline)
          (setq erg (indent-to-column (py-compute-indentation))))
      (beginning-of-line)
      (insert-char ?\n 1)
      ;; Never indent an existing line further than it already is.
      (setq erg (move-to-column (apply #'min
                                       (py-compute-indentation)
                                       (when (not (looking-at-p "[ \t]+$"))
                                         (list (current-indentation)))))))
    (indent-line-to erg)
    (when (interactive-p) (message "%s" erg))
    erg))

;; Allow autopair to support python's triple quotes
(eval-after-load "autopair"
  '(progn
     (add-hook 'python-mode-hook
               (lambda ()
                 (setq autopair-handle-action-fns
                       (list #'autopair-default-handle-action
                             #'autopair-python-triple-quote-action))))))

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(eval-after-load "auto-complete"
  '(progn
     (ac-ropemacs-initialize)
     (add-hook 'python-mode-hook
               (lambda ()
                 (add-to-list 'ac-sources 'ac-source-ropemacs)))))

;; ;; Pylookup

;; ;; load pylookup when compile time
;; (require 'pylookup)
;; ;; (eval-when-compile (require 'pylookup))
;; (setq pylookup-dir (file-name-directory (find-library-name "pylookup")))

;; ;; set executable file and db file
;; (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
;; (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; ;; to speedup, just load it on demand
;; ;; (autoload 'pylookup-lookup "pylookup"
;; ;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)
;; ;; (autoload 'pylookup-update "pylookup"
;; ;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)

;; (define-key py-mode-map (kbd "C-c h") 'pylookup-lookup)

;; Flymake
;; (eval-after-load "flymake"
;;   '(progn
;;      (defun flymake-pylint-init ()
;;        (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                           'flymake-create-temp-inplace))
;;               (local-file (file-relative-name
;;                            temp-file
;;                            (file-name-directory buffer-file-name))))
;;          (list "epylint" (list local-file))))

;;      (add-to-list 'flymake-allowed-file-name-masks
;;                   '("\\.py\\'" flymake-pylint-init))))
