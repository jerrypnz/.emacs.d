(eval-when-compile
  (require 'use-package))

(use-package eyebrowse
  :config
  (progn
    (setq eyebrowse-new-workspace 'counsel-projectile-switch-project)
    (setq eyebrowse-tagged-slot-format "%s [%t]")
    (eyebrowse-mode t)))

(use-package jp-window-layouts
  :bind
  ("M-m l" . jp-window-layouts/body))

(provide 'jp-eyebrowse)
