(require 'diminish)

(defun diminish-undo (mode)
  "Restore mode-line display of diminished mode MODE to its minor-mode value.
Do nothing if the arg is a minor mode that hasn't been diminished.

Interactively, enter (with completion) the name of any diminished mode (a
mode that was formerly a minor mode on which you invoked M-x diminish).
To restore all diminished modes to minor status, answer `diminished-modes'.
The response to the prompt shouldn't be quoted.  However, in Lisp code,
the arg must be quoted as a symbol, as in (diminish-undo 'diminished-modes)."
  (interactive
   (if diminished-mode-alist
       (list (read (completing-read
                    "Restore what diminished mode: "
                    (cons (list "diminished-modes")
                          (mapcar (lambda (x) (list (symbol-name (car x))))
                                  diminished-mode-alist))
                    nil t nil 'diminish-history-symbols)))
     (error "No minor modes are currently diminished.")))
  (if (eq mode 'diminished-modes)
      (let ((diminished-modes diminished-mode-alist))
        (while diminished-modes
          (diminish-undo (caar diminished-modes))
          (callf cdr diminished-modes)))
    (let ((minor      (assq mode      minor-mode-alist))
          (diminished (assq mode diminished-mode-alist)))
      (or minor
          (error "%S is not currently registered as a minor mode" mode))
      (when diminished
        (setq diminished-mode-alist (remove diminished diminished-mode-alist))
        (setcdr minor (cdr diminished))))))

(defun diminish-setup (symbol newlist)
  (set-default symbol newlist)
  ;; Un-diminish all modes
  (diminish-undo 'diminished-modes)
  ;; Diminish each mode the new list
  (mapc (lambda (x) (when (assq (car x) minor-mode-alist)
                      (message "Diminishing %S" x)
                      (diminish (car x) (cdr x))))
        newlist))

(defcustom diminished-minor-modes '()
  "Minor modes to be diminished, and their diminished text, if any."
  :group 'diminish
  :type '(alist :key-type (symbol :tag "Mode"
                                  :match (lambda (w v)
                                           (or (null v)
                                               (assq v minor-mode-alist)))
                                  )
                :value-type (choice :tag "To What"
                                    (const :tag "Hide completely" "")
                                    (string :tag "Abbreviation")))
  :set 'diminish-setup)

(defun diminish-init ()
  (diminish-setup 'diminished-minor-modes diminished-minor-modes))

(eval-after-load "init"
  '(diminish-init))
