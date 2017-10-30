;;; jp-flycheck.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; flycheck
(use-package flycheck
  :ensure t ; load with package.el
  :defer 1
  :commands (flycheck-mode global-flycheck-mode)
  :config
  (progn
    (defun jp--flycheck-display-errors-function (errors)
      (mapc (lambda (err)
              (message "FlyC: %s" (flycheck-error-message err)) (sit-for 1))
            errors))
    (setq flycheck-highlighting-mode 'symbols
          flycheck-indication-mode nil
          flycheck-display-errors-function 'jp--flycheck-display-errors-function
          flycheck-emacs-lisp-load-path 'inherit)
    (setq-default flycheck-disabled-checkers '(c/c++-clang))))

(provide 'jp-flycheck)
;;; jp-flycheck.el ends here
