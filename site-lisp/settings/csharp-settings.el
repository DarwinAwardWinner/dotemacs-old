;;; Import C# mode
;; http://mfgames.com/linux/docs/csharp-mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
(append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(defun csharp-set-style-hook nil
  "set c-file-style for c# files"
  (interactive)
  (c-set-style "bsd"))

(add-hook 'csharp-mode-hook 'csharp-set-style-hook)
