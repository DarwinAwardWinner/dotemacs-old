(require 'python-mode)

(define-key py-mode-map (kbd "C-c C-m") nil)
(define-key py-mode-map (kbd "C-<backspace>") nil)

(defadvice py-indent-line (around save-excursion activate)
  "Prevent [TAB] from moving point to the beginning of the line."
  (if (<= (point) (+ (point-at-bol) (current-indentation)))
      ad-do-it
    (save-excursion
      (goto-char (+ (point-at-bol) (current-indentation)))
      ad-do-it)))

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

;; Pylookup

;; load pylookup when compile time
(require 'pylookup)
;; (eval-when-compile (require 'pylookup))
(setq pylookup-dir (file-name-directory (find-library-name "pylookup")))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; to speedup, just load it on demand
;; (autoload 'pylookup-lookup "pylookup"
;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)
;; (autoload 'pylookup-update "pylookup"
;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)

(define-key py-mode-map (kbd "C-c h") 'pylookup-lookup)

;; Flymake
(eval-after-load "flymake"
  '(progn
     (defun flymake-pylint-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "epylint" (list local-file))))

     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-pylint-init))))
