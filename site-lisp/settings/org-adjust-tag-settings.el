(require 'org-macs)
(require 'cl)

(defcustom org-tags-column-auto-adjust t
  "If t, adjust org-tags-column automatically.
This will keep tags right-justified when the window size
changes. The configured value of org-tags-column will still be
used in saving org files."
  :group 'org-tags
  :type 'boolean)

(defcustom org-tags-column-auto-adjust-delay 0.5
  "Delay after window resizing before tags are readjusted.
If another resize happens within the delay, the timer is
reset. This allows emacs to wait for multiple resize events in rapid succession to occur without readjusing tags at each one."
  :group 'org-tags
  :type '(number :tag "Seconds"))

(defun org-tags-column-do-readjust ()
  "Calls (org-set-tags 1 t), but doesn't change anything else.
Keeps point and mark, and doesn't change (buffer-modified-p).
Also suppresses any messages."
  (let ((b-m-p (buffer-modified-p)))
    (flet ((message (&rest ignored) nil)) ; disable `message'
      (ignore-errors (save-excursion (beginning-of-buffer) (org-set-tags 1 t))))
    (set-buffer-modified-p b-m-p)))

(defun org-tags-column-readjust (&optional adjust-target)
  "Readjust tags in current-buffer to ADJUST-TARGET.
When ADJUST-TARGET is:
 nil, adjust org-tags-column to be right-justified in the window;
 a number, use that as org-tags-column;
 'default, use the global value of org-tags-column;
 anything else, leave org-tags-column unchanged, but still readjust tags to it."
  (when (and org-tags-column-auto-adjust
             (eql major-mode 'org-mode)
             (not (string= (buffer-name) "*Remember*")))
    (org-set-local
     'org-tags-column
     (cond
      ((null adjust-target)
       (- 4 (window-width)))
      ((numberp adjust-target)
       adjust-target)
      ((equal adjust-target 'default)
       (default-value 'org-tags-column))))
     (org-tags-column-do-readjust)))

(defun org-tags-column-readjust-before-save ()
  "Adjust tags back to standard column in preparation for saving."
  (when org-tags-column-auto-adjust
    (org-tags-column-readjust 'default)))

(defun org-tags-column-readjust-after-save ()
  "Adjust tags back to standard column in preparation for saving."
  (when org-tags-column-auto-adjust
    (org-tags-column-readjust nil)))

(defun combine-into-list (args)
  "Combine a list of elements in the way that apply does.
For example, (combine-into-list '(1 2 '(3 4)) yields '(1 2 3 4).
Meant to be used like this:

 (defun my-func (&rest args)
   (apply func (combine-into-list args)))"
  (if (and (null (cdr args))
           (listp (car args)))
      (car args)
    (cons (car args) (combine-into-list (cdr args)))))

(defun run-in-buffer (buf func &rest args)
  (with-current-buffer buf
    (apply func (combine-into-list args))))

(defun run-with-timer-in-current-buffer (secs repeat function &rest args)
  "Identical to run-with-timer, but FUNCTION will run in current buffer, even if you switched buffers in the meantime."
  (interactive "sRun after delay (seconds): \nNRepeat interval: \naFunction: ")
  (run-with-timer secs repeat 'run-in-buffer (current-buffer) function args))

(defvar org-tags-auto-adjust-timer nil
  "Timer that holds the deferred tag adjustment action.
This avoids constantly readjusting tags during resizing, and
instead waits until you're done resizing the window.")

(defun org-tags-column-readjust-later ()
  "Sets up a delayed run of (org-tags-column-readjust).
The delay is `org-tags-column-auto-adjust-delay', which you can
customize. If such a timer was already set, cancel and reset it."
  (when (and org-tags-column-auto-adjust
             (eql major-mode 'org-mode)
             (not (string= (buffer-name) "*Remember*")))
    ;; Cancel the old timer
    (when (timerp org-tags-auto-adjust-timer)
      (cancel-timer org-tags-auto-adjust-timer))
    ;; Start the new timer
    (setq org-tags-auto-adjust-timer
          (run-with-timer-in-current-buffer
           org-tags-column-auto-adjust-delay   ; delay
           nil                                 ; no repeat
           'org-tags-column-readjust)))) ; function & args

; automatically adjust tags on right-hand side
(add-hook 'before-save-hook 'org-tags-column-readjust-before-save)
(add-hook 'after-save-hook 'org-tags-column-readjust-after-save)

(add-hook 'window-configuration-change-hook
          'org-tags-column-readjust-later)

