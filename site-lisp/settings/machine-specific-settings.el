;; Settings for computers in the Salomon lab

(case (intern system-name)

  ;; My laptop
  (aeolus nil)

  ;; Salomon workstation
  (ubuntu
   (set-face-attribute 'default nil :height 105)))
