;; (require 'tabkey2)

;; (defun make-nil-list (length)
;;   "Make a list of nils LENGTH items long"
;;   (when (> length 0)
;;     (cons nil (make-nil-list (- length 1)))))

;; (defun pad-list (length list)
;;   "Pad LIST to LENGTH with by appending nil items"
;;   (if (null (cdr list))
;;       (setcdr list (make-nil-list (- length 1)))
;;     (pad-list (- length 1) (cdr list)))
;;   list)

;; (defcustom tabkey2-completion-functions
;;   '(;; Temporary things
;;     ("Spell check word" flyspell-correct-word-before-point)
;;     ;; Snippets
;;     ("Yasnippet" yas/expand yas/expandable-at-point)
;;     ;; Main mode related, often used
;;     ("Semantic Smart Completion" senator-complete-symbol senator-minor-mode)
;;     ("Programmable completion" pcomplete)
;;     ("nXML completion" nxml-complete)
;;     ("Complete Emacs symbol" lisp-complete-symbol)
;;     ("Widget complete" widget-complete)
;;     ("Comint Dynamic Complete" comint-dynamic-complete)
;;     ("PHP completion" php-complete-function)
;;     ("Tags completion" complete-symbol)
;;     ;; General word completion
;;     ("Predictive word" complete-word-at-point predictive-mode)
;;     ("Predictive abbreviations" pabbrev-expand-maybe)
;;     ("Dynamic word expansion" dabbrev-expand nil (lambda nil (setq dabbrev--last-abbrev-location nil)))
;;     ("Ispell complete word" ispell-complete-word)
;;     ;; The catch all
;;     ("Anything" anything (lambda nil (commandp 'anything)))
;;   )
;;     "List of completion functions.
;; The first 'active' entry in this list is normally used during the
;; 'Tab completion state' by `tabkey2-complete'.  An entry in the
;; list should have either of this forms

;;   \(TITLE COMPLETION-FUNCTION ACTIVE-FORM RESET-FORM)

;; TITLE to show in menus etc.

;; COMPLETION-FUNCTION is the completion function symbol.

;; The entry is considered active if the symbol COMPLETION-FUNCTION
;; is bound to a command and

;;   - This function has a key binding at point.

;; or

;;   - The elisp expression ACTIVE-FORM evaluates to non-nil.  If it
;;   is a single symbol then its variable value is used, otherwise
;;   the elisp form is evaled.

;; RESET-FORM is used to reset the completion function before
;; calling it.

;; When choosing with `tabkey2-cycle-completion-functions'
;; only the currently active entry in this list are shown."
;;     :type '(repeat (list (string :tag "Label")
;;                          (symbol  :tag "Command")
;;                          (choice :tag "Activation criterion"
;;                                  (const :tag "Key Binding at point" nil)
;;                                  (variable :tag "Boolean variable")
;;                                  (function :tag "Boolean function"))
;;                          (choice :tag "Reset function"
;;                                  (const :tag "None" nil)
;;                                  (function :tag "Function"))))
;;     :group 'tabkey2
;;     :set #'(lambda (sym val)
;;              (mapc (apply-partially 'pad-list 4) val)
;;              (set-default sym val)))

;; (defcustom tabkey2-modes-that-just-complete
;;   '(shell-mode)
;;   "Tab is only used for completion in these modes.
;; Therefore `tabkey2-first' just calls the function on Tab."
;;   :type '(repeat symbol)
;;   :group 'tabkey2)

;; (defcustom tabkey2-modes-that-use-more-tabs
;;   '(python-mode
;;     haskell-mode
;;     makefile-mode
;;     org-mode
;;     Custom-mode
;;     ;; other
;;     cmd-mode
;;     )
;;   "In those modes use must use S-Tab to start completion state.
;; In those modes pressing Tab several types may make sense so you
;; can not go into 'Tab completion state' just because one Tab has
;; been pressed.  Instead you use S-Tab to go into that state.
;; After that Tab does completion.

;; You can do use S-Tab in other modes too if you want too."
;;   :type '(repeat symbol)
;;   :group 'tabkey2)

