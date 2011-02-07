;; Scroll horizontally by half screens by default
(defadvice scroll-left (before change-default-arg activate)
  (unless arg
    (setq arg (/ (window-width) 2))))

(defadvice scroll-right (before change-default-arg activate)
  (unless arg
    (setq arg (/ (window-width) 2))))
