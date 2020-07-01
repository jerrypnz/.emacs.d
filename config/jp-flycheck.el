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

  :hook
  ((prog-mode
    css-mode
    sgml-mode
    html-mode
    inferior-emacs-lisp-mode
    lisp-interaction-mode) . flycheck-mode)

  :config
  (progn
    (setq flycheck-highlighting-mode 'symbols
          flycheck-indication-mode nil
          flycheck-emacs-lisp-load-path 'inherit)

    ;; Disable some checkers for certain modes
    (add-hook 'rust-mode-hook
              (lambda ()
                (setq flycheck-disabled-checkers '(rust-cargo rust-clippy))))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (window-height   . 0.25)))))

(use-package flycheck-posframe
  :straight t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults)
  (setq flycheck-posframe-border-width 5))

(use-package flycheck-package
  :straight t
  :after (flycheck)
  :config
  (flycheck-package-setup))

(provide 'jp-flycheck)
;;; jp-flycheck.el ends here
