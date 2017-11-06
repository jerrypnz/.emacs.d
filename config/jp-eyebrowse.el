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
    ;; Switch to a project and use its name (shorten) as the tag of
    ;; the window config TODO `shorten-directory' is defined in
    ;; `jp-modeline.el' - move it to a common lib
    (defun jp-eyebrowse-new-project-workspace ()
      (interactive)
      (let ((project-name (counsel-projectile-switch-project)))
        (when (> (length project-name) 0)
          (eyebrowse-rename-window-config (eyebrowse--get 'current-slot)
                                          (shorten-directory project-name 32)))))

    (setq eyebrowse-new-workspace 'jp-eyebrowse-new-project-workspace)
    (setq eyebrowse-tagged-slot-format "%s [%t]")
    ;; I don't really use its default key map
    (define-key eyebrowse-mode-map eyebrowse-keymap-prefix nil)
    (eyebrowse-mode t)))

(use-package jp-layouts
  :bind
  ("M-m l" . jp-layouts/body))

(provide 'jp-eyebrowse)
;;; jp-eyebrowse.el ends here
