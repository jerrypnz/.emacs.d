;;; jp-eyebrowse.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package eyebrowse
  :config
  (progn
    ;; Switch to a project and use its name as the tag of
    ;; the window config
    (defun jp-eyebrowse-switch-project ()
      (interactive)
      (let ((project-name (counsel-projectile-switch-project)))
        (when (> (length project-name) 0)
          (eyebrowse-rename-window-config
           (eyebrowse--get 'current-slot)
           ;;(shorten-directory project-name 32)
           (file-name-nondirectory (directory-file-name project-name))))))

    (eval-after-load "counsel-projectile"
      '(define-key counsel-projectile-mode-map [remap projectile-switch-project] #'jp-eyebrowse-switch-project))

    (setq eyebrowse-new-workspace 'jp-eyebrowse-switch-project)
    (setq eyebrowse-tagged-slot-format "%s [%t]")
    ;; I don't really use its default key map
    (define-key eyebrowse-mode-map eyebrowse-keymap-prefix nil)
    (eyebrowse-mode t)))

(use-package jp-layouts
  :bind
  ("M-m l" . jp-layouts/body))

(provide 'jp-eyebrowse)
;;; jp-eyebrowse.el ends here
