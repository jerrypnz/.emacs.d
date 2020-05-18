;;; jp-treemacs.el --- Treemacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'major-mode-hydra)

(use-package treemacs
  :straight t
  :config
  (progn
    (setq treemacs-follow-after-init t
          treemacs-is-never-other-window t
          treemacs-project-follow-cleanup t
          treemacs-tag-follow-delay 0.2
          treemacs-collapse-dirs 3
          treemacs-width 40)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-git-mode 'simple)
    (treemacs-fringe-indicator-mode t)))

(use-package treemacs-projectile
  :straight t
  :after treemacs projectile)

(use-package treemacs-magit
  :straight t
  :after treemacs magit)

(provide 'jp-treemacs)
;;; jp-treemacs.el ends here
