; -*- mode: emacs-lisp; -*-
(require 'tempbuf)
(require 'misc-settings)

(defun mode-symbol (sym)
  "Append \"-mode\" to SYM unless it already ends in it."
  (let ((symname (symbol-name sym)))
    (intern
     (concat symname
             (unless (string-ends-with "-mode" symname)
               "-mode")))))

(defun tempbuf-major-mode-hook ()
  "Turn on `tempbuf-mode' in current buffer if buffer's `major-mode' is in `tempbuf-temporary-major-modes'.

Else turn off `tempbuf-mode'."
  (if (memq major-mode tempbuf-temporary-major-modes)
      (turn-on-tempbuf-mode)
    (turn-off-tempbuf-mode)))

(defun tempbuf-setup-temporary-major-modes (symbol newval)
  (set-default symbol (mapcar 'mode-symbol newval))
  ;; Set tempbuf-mode correctly in existing buffers.
  (mapc (lambda (buf)
          (with-current-buffer buf
            (tempbuf-major-mode-hook)))
        (buffer-list)))

(defcustom tempbuf-temporary-major-modes nil
  "Major modes in which `tempbuf-mode' should be activated.

This will cause buffers of these modes to be automatically killed
if they are inactive for a short while."
  :group 'tempbuf
  :set 'tempbuf-setup-temporary-major-modes
  :type '(repeat (symbol :tag "Mode")))

(add-hook 'after-change-major-mode-hook 'tempbuf-major-mode-hook)

(add-hook 'ess-help-mode-hook 'tempbuf-major-mode-hook)
