;;; jp-ivy.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

;; ivy
(use-package ivy
  :bind
  ("M-m r" . ivy-resume)

  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")))

(use-package ivy-hydra)

(use-package counsel
  :bind
  (("M-x"     . counsel-M-x)
   ("M-y"     . counsel-yank-pop)
   ("C-s"     . counsel-grep-or-swiper)
   ("C-x C-f" . counsel-find-file)
   ("C-h f"   . counsel-describe-function)
   ("C-h v"   . counsel-describe-variable))

  :config
  (progn
    (setq counsel-grep-base-command
          "rg -i -M 120 --no-heading --line-number --color never '%s' %s")))

(use-package jp-ivy-utils
  :bind
  ("C-M-s" . jp-counsel-grep-or-swiper-symbol-at-pt))

(provide 'jp-ivy)
;;; jp-ivy.el ends here
