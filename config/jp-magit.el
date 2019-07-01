;;; jp-magit.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; magit
(use-package magit
  :straight t
  :defer 2
  :init
  ;; Since we're using git subtree without building it with Make, we
  ;; don't have "magit-version.el" file.  Get around it by setting the
  ;; version here explicitly.
  (setq magit-version "2.11.0")

  :config
  (setq magit-repository-directories
        '(("~/dev/workspace/"   . 3)
          ("~/dev/personal/"    . 2)
          ("~/dev/open-source/" . 2)))

  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package magit-todos
  :straight t
  :after (magit)
  :config
  (progn
    (setq magit-todos-require-colon nil)
    (magit-todos-mode)))

;; git-timemachine
(use-package git-timemachine
  :straight t
  :commands (git-timemachine))

;; diff-hl
(use-package diff-hl
  :straight t
  :config
  (progn
    (add-hook 'iedit-mode-hook #'config-git--diff-hl-mode-on)
    (add-hook 'iedit-mode-end-hook #'config-git--diff-hl-mode-off)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (global-diff-hl-mode)))

(provide 'jp-magit)
;;; jp-magit.el ends here
