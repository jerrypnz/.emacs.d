;;; jp-projectile.el --- Projectile config.  -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package))

(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "M-m p"))
  
  :config
  (projectile-mode))

(use-package counsel-projectile
  :bind
  (:map projectile-command-map
        ("/" . counsel-projectile-rg))
  :config
  (counsel-projectile-on))

(use-package jp-projectile-utils
  :bind
  (("C-x b" . jp-switch-buffer)
   ("M-m /" . jp-search)))

(provide 'jp-projectile)
