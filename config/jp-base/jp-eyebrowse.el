(eval-when-compile
  (require 'use-package))

(use-package eyebrowse
  :init
  (setq eyebrowse-keymap-prefix (kbd "M-m w"))

  :config
  (eyebrowse-mode t))

(provide 'jp-eyebrowse)
