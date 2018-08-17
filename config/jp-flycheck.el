;;; jp-flycheck.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; flycheck
(use-package flycheck
  :straight t
  :defer 1
  :commands (flycheck-mode global-flycheck-mode)
  :config
  (progn

    ;; (defun jp--flycheck-display-errors-function (errors)
    ;;   (mapc (lambda (err)
    ;;           (message "FlyC: %s" (flycheck-error-message err)) (sit-for 1))
    ;;         errors))
    ;; (setq flycheck-display-errors-function 'jp--flycheck-display-errors-function)

    (setq flycheck-highlighting-mode 'symbols
          flycheck-indication-mode nil
          flycheck-emacs-lisp-load-path 'inherit)
    (setq-default flycheck-disabled-checkers '(c/c++-clang emacs-lisp-checkdoc))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (window-height   . 0.25)))))

(use-package flycheck-pos-tip
  :straight t
  :after (flycheck)
  :config
  (flycheck-pos-tip-mode))

(provide 'jp-flycheck)
;;; jp-flycheck.el ends here
