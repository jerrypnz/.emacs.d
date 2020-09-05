;;; jp-hydra.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; Major mode keys managed by a pretty hydra
(use-package major-mode-hydra
  :straight (:host github :type git :repo "jerrypnz/major-mode-hydra.el" :branch "develop")
  :demand t
  :bind
  (("C-M-SPC" . major-mode-hydra))

  :config
  (progn
    (setq major-mode-hydra-invisible-quit-key "q")
    (defun jp-major-mode-hydra-title-generator (_)
      `(with-mode-icon major-mode
                       (propertize (s-concat (format-mode-line mode-name) " Commands")
                                   'face '(:weight bold :height 1.1))
                       1.1))

    (setq major-mode-hydra-title-generator #'jp-major-mode-hydra-title-generator)))

(provide 'jp-hydra)
;;; jp-hydra.el ends here
