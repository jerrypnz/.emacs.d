;;; jp-eyebrowse.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package eyebrowse
  :straight t
  :config
  (progn
    (setq eyebrowse-tagged-slot-format "%s [%t]")
    ;; I don't really use its default key map
    (define-key eyebrowse-mode-map eyebrowse-keymap-prefix nil)
    (eyebrowse-mode t)))

(use-package jp-layouts
  :after (eyebrowse)
  :config
  (progn
    (setq eyebrowse-new-workspace 'jp-eyebrowse-switch-project)))

(provide 'jp-eyebrowse)
;;; jp-eyebrowse.el ends here
