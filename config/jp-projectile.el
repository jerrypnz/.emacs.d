;;; jp-projectile.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; counsel-projectile also loads projectile itself
(use-package counsel-projectile
  :init
  (setq projectile-keymap-prefix (kbd "M-m p"))

  :commands (counsel-projectile
             counsel-projectile-switch-project
             counsel-projectile-find-file
             counsel-projectile-find-dir)

  :bind
  (:map projectile-command-map
        ("/" . counsel-projectile-rg))

  :config
  (progn
    (setq counsel-projectile-remove-current-buffer t)
    (setq counsel-projectile-remove-current-project t)
    (projectile-mode)
    (counsel-projectile-mode)))

(use-package jp-projectile-utils
  :bind
  (("C-x b" . jp-switch-buffer)
   ("M-m /" . jp-search)
   ("M-m *" . jp-search-symbol-at-pt)))

(use-package ibuffer-projectile
  :config
  (progn
    (setq ibuffer-projectile-prefix "Project: ")
    (add-hook 'ibuffer-hook
              (lambda ()
                (ibuffer-projectile-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))))))

(provide 'jp-projectile)

(require 'counsel-projectile)
;;; jp-projectile.el ends here
