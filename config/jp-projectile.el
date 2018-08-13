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
  :init
  (setq projectile-keymap-prefix (kbd "M-m p"))

  :defer nil

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

    (defun jp-refresh-projectile-projects ()
      (when (require 'magit nil t)
        (projectile-cleanup-known-projects)
        (mapc #'projectile-add-known-project
              (mapcar #'file-name-as-directory (magit-list-repos)))))

    (advice-add #'counsel-projectile-switch-project :before #'jp-refresh-projectile-projects)

    (projectile-mode)
    (counsel-projectile-mode)))

(use-package all-the-icons-ivy
  :straight t
  :after (counsel-projectile)
  :config
  (progn
    (setq all-the-icons-ivy-file-commands
          '(counsel-find-file
            counsel-file-jump
            counsel-recentf
            counsel-projectile
            counsel-projectile-find-file
            counsel-projectile-find-dir))
    (all-the-icons-ivy-setup)))

(use-package jp-projectile-utils
  :after (counsel-projectile)
  :bind
  (("C-x b" . jp-switch-buffer)
   ("M-m b" . jp-switch-buffer)
   ("M-m f" . jp-open-file)
   ("M-m /" . jp-search)
   ("M-m *" . jp-search-symbol-at-pt))

  :config
  (progn
    (which-key-add-key-based-replacements
      "M-m /" "search"
      "M-m *" "search-symbol-at-point"
      "M-m b" "buffers"
      "M-m f" "find-file")))

(use-package ibuffer-projectile
  :straight t
  :config
  (progn
    (setq ibuffer-projectile-prefix "Project: ")
    (add-hook 'ibuffer-hook
              (lambda ()
                (ibuffer-projectile-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))))))

(provide 'jp-projectile)
;;; jp-projectile.el ends here
