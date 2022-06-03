;;; jp-projectile.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; projectile also loads projectile itself
(use-package projectile
  :straight t
  :defer nil
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-enable-idle-timer t)
    (setq projectile-track-known-projects-automatically nil)
    (projectile-mode)))

(use-package consult-projectile
  :straight t)

(use-package jp-projectile-utils
  :after (projectile)
  :bind (("C-x b" . jp-switch-buffer)
         ("M-i" . jp-switch-buffer)))

(provide 'jp-projectile)
;;; jp-projectile.el ends here
