;;; jp-ivy.el --- Configuration for Ivy.  -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package))

(use-package ivy
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")))

(use-package ivy-hydra)

(use-package counsel
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-h f"   . counsel-describe-function)
   ("C-h v"   . counsel-describe-variable)))

(use-package swiper
  :bind
  (("C-s" . swiper)))

(provide 'jp-ivy)

