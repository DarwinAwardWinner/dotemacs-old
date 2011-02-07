(autoload 'tmenu-menubar "tmenu"
     "Text based interface to the menubar."
             t nil)
(global-set-key [f10] 'tmenu-menubar)

(eval-after-load 'ido
  '(defadvice tmenu-menubar (around disable-ido activate)
     "Disable ido in tmenu"
     (let ((ido-everywhere nil))
       ad-do-it)))
