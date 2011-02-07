;; Apropos search noninteractive things too
;;(substitute-key-definition 'apropos-command 'apropos global-map)
;; Home goes to first non-whitespace on line
;; (substitute-key-definition 'move-beginning-of-line 'back-to-indentation global-map)

;; Add keybindings for useful stuff
(global-set-key (kbd "<f8>") 'bury-buffer)
(global-set-key (kbd "C-c j") 'join-line)
(define-key emacs-lisp-mode-map [(control tab)] 'lisp-complete-symbol)

;; Make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; No EOL whitespace; helps make buffers diff correctly
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Killing a buffer also kills its windows
(require 'misc-cmds)

;; "M-x reload-buffer" will revert-buffer without requiring confirmation
(defun reload-buffer ()
  "revert-buffer without confirmation"
  (interactive)
  (revert-buffer t t))

(defun reinit ()
  "Reload init.el"
  (interactive)
  (load-file (expand-file-name "init.el" "~/.emacs.d")))

(defun tj-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (message "Buffer is set to read-only because it is large.  Undo also disabled.")))

(add-hook 'find-file-hooks 'tj-find-file-check-make-large-file-read-only-hook)

(defun my-help ()
  "If thing at point is a:
function, tries to `describe-function';
variable, uses 'describe-variable';
otherwise uses `manual-entry' to display
manpage of a `current-word'."
  (interactive)
  (let ((var (variable-at-point)))
    (if (symbolp var)
        (describe-variable var)
      (let ((fn (function-called-at-point)))
        (if fn
            (describe-function fn)
          (man (current-word)))))))

(global-set-key (kbd "<C-f1>") 'my-help)


;; C-mwheel for zooming, like other apps
(global-set-key (kbd "<C-mouse-4>")
                (lambda () (interactive) (text-scale-increase  1)))
(global-set-key (kbd "<C-mouse-5>")
                (lambda () (interactive) (text-scale-increase -1)))
(global-set-key (kbd "<C-down-mouse-2>")
                (lambda () (interactive) (text-scale-increase  0)))

(defun set-default-face-after-making-x-frame (frame)
  "Used to work around the limitations of emacs --daemon, which breaks when you set the default face via custom-set-faces."
  (when window-system (set-face-attribute 'default nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal :height 103 :width 'normal :foundry "unknown" :family "Droid Sans Mono")))

;; (message nil) clears the minibuffer, restoring the prompt
(add-hook 'auto-save-hook #'(lambda () (message nil)) 'append)

(defun insert-key (key)
  (interactive (list (read-key-sequence "Key: ")))
  (insert "(kbd \"" (key-description key) "\")"))

(add-hook 'text-mode-hook #'(lambda nil (visual-line-mode 1)))

(defun unslantify-quotes-in-buffer ()
  (interactive)
  (save-excursion
     (mapc #'(lambda (char)
               (beginning-of-buffer)
               (while (search-forward (concat (vector char)) nil t)
                 (replace-match "\"")))
           "“”")))

(defadvice delete-trailing-whitespace (around prevent-at-point activate)
  "Prevent delete-trailing-whitespace from deleting whitespace the the left of point, on the point's line"
  (insert "x")
  ad-do-it
  (delete-backward-char 1))

(defun split-window-into-new-frame (&optional win frm)
  "Split the current (or specified) window into a separate frame.

Also delete that window from the current frame. If there is only
one window in the current frame to begin with, then do nothing.

If the specified window is not from the currently selected frame,
then you must specify that window's frame as well."
  (interactive)
  (let* ((win (or win (selected-window)))
         (frm (or frm (selected-frame)))
         (buf (window-buffer win)))
    ;; Check that win is in frm
    (when (not (memq win (window-list frm)))
      (error "Specified window is not in specified frame."))
    (delete-window win)
    ;; If success, make new frame and select same buffer
    (let ((new-frm (make-frame)))
      (select-frame new-frm)
      (delete-other-windows)
      (switch-to-buffer buf))))

(defun settings-file-stems ()
  "Return a list of names of settings file name stems.

  The trailing `-settings.el' in the file name will be removed,
  leaving only the unique part of the file name."
  (mapcar (apply-partially 'replace-regexp-in-string "-settings.el$" "")
          (remove-if-not (apply-partially 'string-match-p "-settings.el$")
                         (remove-if (apply-partially 'string-match-p "^\\.")
                                    (directory-files init-settings-path)))))

(defun find-settings-file (name)
  "Visit `NAME'-settings.el in `init-settings-path`.

If `NAME' is nil or empty string, open the settings directory."
  (interactive (list (completing-read "Settings file:" (settings-file-stems))))
  (if (or (not name) (= 0 (length name)))
      (find-file init-settings-path)
    (find-file (expand-file-name (concat name "-settings.el") init-settings-path))))

(defun make-list-string (items &optional recursed)
  "Makes an English-style list from a list of strings.

Converts a list of strings into a string that lists the items
separated by commas, as well as the word `and' before the last
item. In other words, returns a string of the way those items
would be listed in english."
  (cond ((not (first items))
         ;; Zero items
         "")
        ((not (rest items))
         ;; One item
         (assert (stringp (first items)))
         (first items))
        ((not (rest (rest items)))
         ;; Two items
         (assert (stringp (second items)))
         ;; Only use a comma for 3 or more items, as indicated by
         ;; recursion.
         (let ((format-string (if recursed
                                  "%s, and %s"
                                "%s and %s")))
           (format format-string (first items) (second items))))
        ;; More than two items: use recursion
        (t
         (format "%s, %s" (first items) (make-list-string (rest items) 'recursed)))))

(defun twiddle-mode (mode)
  "If MODE is activated, then deactivate it and then activate it again.

If MODE is not active, do nothing."
  (when (eval mode)
    (funcall mode 0)
    (funcall mode 1)))

(defun add-to-head-of-list (list-var element)
  "Like `add-to-list', but ELEMENT is guaranteed to be at the
head of the list, regardless of whether it was previously an
element of the list.

In particular, if ELEMENT is already in LIST-VAR, it is removed
and re-added at the head."
  (set list-var (cons element (remove element (symbol-value list-var)))))

(defun string-ends-with (ending string)
  "Return t if the final characters of STRING are ENDING"
  (string-match-p (concat ending "$") string))

(defun directory-list-recursive (dir &optional hidden)
  "Return a list of DIR and all of its subdirectories, recursively.

If HIDDEN is t, include hidden directories. Paths returned are
all absolute. if DIR is a file, an empty list is returned."
  (when (file-directory-p dir)
    (directory-list-recursive-internal (expand-file-name dir) hidden)))

(defun directory-list-recursive-internal (dir hidden)
  "Don't use this function directly. See `directory-list-recursive'."
  (assert (file-name-absolute-p dir))
  (let ((subdirs (remove-if-not (lambda (subdir)
                                  (and (file-directory-p subdir)
                                       (not (member (file-name-nondirectory subdir) '("." "..")))))
                                (directory-files dir 'full nil 'nosort))))
    (when (not hidden)
      (setq subdirs
            (remove-if (lambda (subdir)
                         (string= "." (substring (file-name-nondirectory subdir) 0 1)))
                       subdirs)))
    (cons dir (mapcan (lambda (subdir) (directory-list-recursive-internal subdir hidden))
                      subdirs))))

(defun clean-stale-elc-files (dir &optional recursive hidden)
  "Delete elc files in DIR that do not have corresponding el files."
  (dolist (dir (if recursive
                   (directory-list-recursive dir hidden)
                 (list dir)))
    (dolist (elc (directory-files dir 'full "\\.elc$" 'nosort))
      (unless (file-exists-p (concat (file-name-sans-extension elc) ".el"))
        (message "Cleaning stale elc file %s" elc)
        (delete-file elc)))))

(provide 'misc-settings)
