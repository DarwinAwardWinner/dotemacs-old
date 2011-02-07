(eval-after-load 'savehist
  '(progn
     ;; Redefined because the original definition didn't allow nil in
     ;; custom
     (defcustom savehist-autosave-interval (* 5 60)
       "The interval between autosaves of minibuffer history.
If set to nil, disables timer-based autosaving."
       :type '(choice (const :tag "Disabled" nil)
                      (integer :tag "Seconds" 300))
       :group 'savehist)

     (add-hook 'auto-save-hook 'savehist-autosave)

     ;;   (defvar savehist-temp-disable nil
     ;;     "When t, disable savehist-save.
     ;; Used to disable savehist during ido-completing-read")

     (defadvice ido-completing-read (around disable-savehist-in-ido activate)
       "Savehist chokes while ido is active, so disable it."
       (let ((old-savehist-status savehist-mode))
         (savehist-mode 0)
         ad-do-it
         (when old-savehist-status
           (savehist-mode 1))))

  ;; (let ((savehist-temp-disable t)) ad-do-it))

  ;; (defadvice savehist-save (around disable-savehist-in-ido activate)
  ;;   (unless savehist-temp-disable ad-do-it))

))