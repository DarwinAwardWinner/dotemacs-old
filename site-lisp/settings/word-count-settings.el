(defun wc (&optional start end)
  "Prints number of lines, words and characters in region or whole buffer."
  (interactive)
  (let ((n 0)
        (start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
    (let ((word-count n)
          (line-count (count-lines start end))
          (char-count (- end start)))
      (message "%3d lines; %3d words; %3d characters" line-count word-count char-count))))
