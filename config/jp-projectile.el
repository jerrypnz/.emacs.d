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
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))

  :config
  (progn
    (setq projectile-track-known-projects-automatically nil)
    (projectile-mode)))

(use-package jp-projectile-utils
  :after (projectile)
  :bind (("C-x b" . jp-switch-buffer)
         ("M-i" . jp-switch-buffer)))

(provide 'jp-projectile)
;;; jp-projectile.el ends here
