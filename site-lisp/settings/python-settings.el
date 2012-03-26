(require 'python-mode)


(eval-after-load "smart-compile+"
  '(add-to-list 'smart-compile-alist
                '("\\.py$" . "epylint %f | sort -t: -k2 -g")))

(define-key python-mode-map (kbd "C-c C-m") nil)
(define-key python-mode-map (kbd "C-<backspace>") nil)

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

(defcustom py-electric-comment-dedent-only nil
  "Only allow py-electric-comment to dedent a line, not indent it further."
  :type 'boolean)

(defadvice py-electric-comment (around dedent-only activate)
  "Only allow py-electric-comment to dedent a line, not indent it further."
  (if py-electric-comment-dedent-only
      (let ((indentation-before (current-indentation)))
        ad-do-it
        (when (> (current-indentation) indentation-before)
          (save-excursion
            (beginning-of-line)
            (delete-forward-char (- (current-indentation) indentation-before)))))
    ad-do-it))

(defadvice py-newline-and-indent (around dont-indent-existing-line activate)
  "Don't increase indentation depth on existing line"
  (let ((original-indentation (current-indentation)))
    ad-do-it
    (ignore-errors
      (when (not (looking-at-p "[ \t]*$"))
        (save-excursion
          (forward-line 0)
          (delete-forward-char (- (current-indentation)
                                  original-indentation)))))))

(defadvice py-newline-and-indent (after goto-end-of-indent activate)
  "After inserting a newline, put point ad end of indentation"
  (goto-char (py-current-indentation)))

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
