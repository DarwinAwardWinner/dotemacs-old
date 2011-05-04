(require 'ido)
(load "ido-other-window" 'noerror)

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read (around use-ido-when-possible activate)
  "If `ido-everywhere' is t, then use ido-completing-read wherever possible.
Even some places where ido doesn't already enable it."
  (if (or (not ido-mode)
          (not ido-everywhere)
          (boundp 'ido-cur-item)) ; Avoid infinite loop from ido calling completing-read
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                                     allcomp
                                     nil require-match initial-input hist def))
        ad-do-it))))

(defadvice find-file-read-args (around disable-ido activate)
  "Disable ido in find-file"
  (let ((ido-everywhere nil)) ad-do-it))

(defadvice ido-complete-space (around handle-require-match activate)
  "If require-match is nil, always insert space."
  (if (bound-and-true-p require-match)
      (ido-complete)
    (insert " ")))

(eval-after-load 'mic-paren
  '(defadvice mic-paren-highlight (around disable-inside-ido activate)
     "Disable mic-paren highlighting in ido"
     (unless (ido-active)
       ad-do-it)))

(provide 'ido-settings)
