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
    (treemacs-tag-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-git-mode 'simple)
    (treemacs-fringe-indicator-mode nil)
    (treemacs-resize-icons 16)
    ;(add-hook 'treemacs-mode-hook '(lambda () (setq mode-line-format nil)))

    (major-mode-hydra-bind treemacs-mode "Basic"
      ("?" treemacs-helpful-hydra/body "helpful"))
    (major-mode-hydra-bind treemacs-mode "Workspace"
      ("wc" treemacs-create-workspace "create workspace")
      ("wo" treemacs-switch-workspace "select workspace")
      ("wD" treemacs-remove-workspace "remove workspace"))
    (major-mode-hydra-bind treemacs-mode"Project"
      ("pp" treemacs-projectile "add project")
      ("pd" treemacs-remove-project-from-workspace "remove project")
      ("pr" treemacs-rename-project "rename project")
      ("pc" treemacs-collapse-project "collapse project")
      ("pC" treemacs-collapse-all-projects "collapse all projects"))))

(use-package treemacs-projectile
  :straight t
  :after (:and treemacs projectile))

(use-package treemacs-magit
  :straight t
  :after (:and treemacs magit))

(provide 'jp-treemacs)
;;; jp-treemacs.el ends here
