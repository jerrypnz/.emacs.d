;;; jp-projectile.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; counsel-projectile also loads projectile itself
(use-package counsel-projectile
  :straight t
  :defer nil
  :commands (counsel-projectile
             counsel-projectile-switch-project
             counsel-projectile-find-file
             counsel-projectile-find-dir)
  :bind (:map projectile-command-map
              ("/" . counsel-projectile-rg))

  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))

  :config
  (progn
    (setq projectile-track-known-projects-automatically nil)
    (setq counsel-projectile-remove-current-buffer t)
    (setq counsel-projectile-remove-current-project t)
    (projectile-mode)
    (counsel-projectile-mode)))

(use-package jp-projectile-utils
  :after (counsel-projectile)
  :bind (("C-x b" . jp-switch-buffer)
         ("M-i" . jp-switch-buffer)))

(provide 'jp-projectile)
;;; jp-projectile.el ends here
