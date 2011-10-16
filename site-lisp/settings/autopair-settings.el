(require 'autopair)

;; Hooks for modes in which autopair should be disabled
(setq autopair-disable-mode-hooks '(inferior-ess-mode-hook))

;; Disable autopair in certain modes
(mapc (lambda (hook)
        (add-hook hook (lambda () (setq autopair-dont-activate t)))
        )
      autopair-disable-mode-hooks)

(defadvice autopair-on (around ignore-errors activate)
  "Don't raise an error when autopair-on fails"
  (ignore-errors ad-do-it))

(setq autopair-autowrap t)              ; Enable autowrapping
(setq autopair-skip-whitespace t)
(autopair-global-mode 1) ;; to enable in all buffers



(defadvice autopair-skip-close-maybe (around indent-new-line activate)
  ;; If the line is blank *before* inserting the character...
  (let ((line-starts-blank
         (save-excursion
           (looking-back "^[ \t]*"))))
    ad-do-it
    ;; Then indent the line *afterward*
    (when line-starts-blank
      (let ((tab-always-indent t))
        (indent-for-tab-command)))))
