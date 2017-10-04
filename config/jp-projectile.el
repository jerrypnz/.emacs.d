(eval-when-compile
  (require 'use-package))

;; counsel-projectile also loads projectile itself
(use-package counsel-projectile
  :init
  (setq projectile-keymap-prefix (kbd "M-p"))

  :commands (counsel-projectile
             counsel-projectile-switch-project
             counsel-projectile-find-file
             counsel-projectile-find-dir)

  :bind
  (:map projectile-command-map
        ("/" . counsel-projectile-rg))

  :config
  (progn
    (projectile-mode)
    (counsel-projectile-on)))

(use-package jp-projectile-utils
  :bind
  (("C-x b" . jp-switch-buffer)
   ("M-m /" . jp-search)))

(provide 'jp-projectile)

(require 'counsel-projectile)
