;; These are settings pertaining to the redo build system. They have
;; nothing to do with reversing the effects of undo operations or
;; repeating previous actions.

(defcustom redo-file-boilerplate
  "#!/bin/sh"
  "Boilerplate text to be added to the beginning of a new perl test file.

A trailing newline will always be added, so you can leave it off."
  :type '(string :tag "Template"))

(defun redo-file-do-setup (&optional buf)
  "Set up a new redo file.

Simply insert `redo-file-boilerplate' if the file is empty, then
call `normal-mode'. Mostly, just add #!/bin/sh to the beginning
so that it gets recognized as a sh-mode file."

  ;; Recursion protect
  (when (not (boundp 'redo-file-already-setup))
    (let (redo-file-already-setup)
      ;; Insert boilerplate if text is empty
      (when (= (buffer-size) 0)
        ;; Boilerplate does not count as modification
        (let ((previous-buffer-modified-state (buffer-modified-p)))
          (insert redo-file-boilerplate)
          (insert "\n")
          (set-buffer-modified-p previous-buffer-modified-state)))

      (sh-mode))))

(add-to-list 'auto-mode-alist '("\\.do" . redo-file-do-setup))
