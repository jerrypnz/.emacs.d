;;; jp-flycheck.el --- Flycheck config.  -*- lexical-binding: t; -*-

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
    (setq flycheck-highlighting-mode nil
          flycheck-display-errors-function 'jp--flycheck-display-errors-function)))

(provide 'jp-flycheck)
