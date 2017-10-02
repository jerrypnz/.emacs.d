;;; jp-flycheck.el --- Configuration for Ivy.  -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package))

(use-package flycheck
  :ensure t ; load with package.el
  :defer 1
  :commands (global-flycheck-mode))

(provide 'jp-flycheck)
