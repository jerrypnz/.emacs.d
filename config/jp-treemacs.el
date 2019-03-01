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
    (setq
     ;; Path to the file treemacs uses to persist its state
     treemacs-persist-file (expand-file-name "treemacs-persist" user-emacs-directory)

     ;; Follow the currently selected file
     treemacs-follow-after-init t

     ;; Prevents treemacs from being selected with `other-window`
     treemacs-is-never-other-window t)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-git-mode 'simple)

    ;; Disable the indicator next to open files--hl-line is sufficient
    (treemacs-fringe-indicator-mode t)

    ;; Define custom key bindings
    (major-mode-hydra-bind treemacs-mode "Basic"
      ("?" treemacs--helpful-hydra "helpful"))

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

(provide 'jp-treemacs)
;;; jp-treemacs.el ends here
