; -*- mode: emacs-lisp; -*-
(require 'tempbuf)
(require 'misc-settings)

(defun concat-symbols (&rest symbols)
  "Like concat, only for symbols instead of strings. Returns a symbol."
  (intern (apply 'concat (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x)) symbols))))

(defun mode-hook-symbols (modes)
  "Make an educated guess as to the name of the mode-hooks being
  referenced in MODES.

  If MODE ends in `-hook', do nothing; if it ends in `-mode',
  append `-hook'; else, append `-mode-hook'. Returns a list of symbols.
  "
  (mapcar (lambda (mode)
            (let* ((mode-string (if (stringp mode) mode (symbol-name mode)))
                   (correct-mode-symbol
                    (cond ((string-ends-with "-hook" mode-string)
                           mode)
                          ((string-ends-with "-mode" mode-string)
                           (concat-symbols mode "-hook"))
                          ((concat-symbols mode "-mode-hook")))))
              correct-mode-symbol))
          modes))

(defun add-function-to-multiple-hooks (fun hooks &optional append local)
  "Take a single function and add it to multiple hooks."
  (mapc (lambda (hook)
          (add-hook hook fun append local))
        hooks))

(defun remove-function-from-multiple-hooks (fun hooks &optional local)
  "Take a single function and remove it from multiple hooks."
  (mapc (lambda (hook)
          (remove-hook hook fun local))
        hooks))

(defun tempbuf-setup-temporary-major-modes (symbol value)
  ;; In case some modes were removed from the list, remove the hook.
  (when (boundp symbol)
    (let ((oldval (eval symbol)))
      (remove-function-from-multiple-hooks 'turn-on-tempbuf-mode
					   (mode-hook-symbols oldval))))
  ;; Now add the hook to current elements of the list
  (when value
    (add-function-to-multiple-hooks 'turn-on-tempbuf-mode
				    (mode-hook-symbols value)))
  ;; Tempify already-existing buffers
  (save-window-excursion
    (mapc (lambda (buf)
            (switch-to-buffer buf)
            (when (memq major-mode value)
              (turn-on-tempbuf-mode)))
          (buffer-list)))
  ;; Lastly, set the value
  (set-default symbol value))

(defcustom tempbuf-temporary-major-modes nil
  "List of major modes that should automatically be marked temporary."
  :group 'tempbuf
  :set 'tempbuf-setup-temporary-major-modes
  :type '(repeat (symbol :tag "Mode")))
