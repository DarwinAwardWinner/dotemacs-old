;; Settings for computers in the Salomon lab

(case (intern system-name)

  ;; My laptop
  (aeolus nil)

  ;; Salomon workstation
  (ubuntu
   (set-face-attribute 'default nil :height 105)
   (setq magit-repo-dirs
         (cons "~/Projects"
               (remove-if (apply-partially 'string-match-p
                                           "~/Projects/")
                          magit-repo-dirs))
         clean-buffer-list-delay-general 7)))
