;;; jp-ui.el --- Configuration related to UI: font, theme, icons etc -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; Doom theme
(use-package doom-themes
  :straight t
  :preface
  (defvar region-fg nil)
  :config
  (progn
     ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
    ;; may have their own settings.
    (doom-themes-org-config)
    ;; Lead star hiding does not play well with ligatures
    (setq org-hide-leading-stars nil
          org-hide-leading-stars-before-indent-mode nil)
    (doom-themes-treemacs-config)
    (setq doom-nord-padded-modeline 2)))

;; Load doom theme through jp-doom-themes
(use-package jp-themes
  :after (doom-themes)
  :config
  (progn
    (setq jp-light-theme 'doom-one-light
          jp-dark-theme 'doom-palenight
          jp-current-theme-dark-p t
          ;;jp-default-font "Fira Code Retina-13"
          jp-default-font "JetBrains Mono Medium-14"
          jp-variable-pitch-font "Lucida Grande-14")
    (jp-themes-load)))

(use-package all-the-icons
  :straight t)

(use-package moody
  :straight t
  :config
  (progn
    (setq x-underline-at-descent-line t)
    (setq moody-slant-function (if (eq window-system 'ns)
                                   #'moody-slant-apple-rgb
                                 #'moody-slant))))

(use-package jp-modeline
  :config
  (jp-modeline-setup))

(provide 'jp-ui)
;;; jp-ui.el ends here
