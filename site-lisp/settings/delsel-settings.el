(require 'misc-settings)
(add-hook 'after-change-major-mode-hook (apply-partially 'twiddle-mode 'delete-selection-mode))
