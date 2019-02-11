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
    (setq doom-nord-padded-modeline 2)
    (load-theme 'doom-nord t)

    (doom-themes-org-config)

    (defmacro jp-doom-themes-set-faces (theme-name &rest faces)
      (declare (indent 1) (doc-string 3))
      `(custom-theme-set-faces
        ,theme-name
        ,@(mapcar #'doom-themes--build-face faces)))

    ;; Customize faces
    (jp-doom-themes-set-faces 'doom-nord
      ;; mode-line
      (mode-line :background modeline-bg
                 :foreground modeline-fg
                 :overline (doom-lighten modeline-bg 0.2)
                 :underline (doom-lighten modeline-bg 0.2))
      (mode-line-inactive :background modeline-bg-inactive
                          :foreground modeline-fg-alt
                          :overline (doom-lighten modeline-bg 0.2)
                          :underline (doom-lighten modeline-bg 0.2))
      (mode-line-read-only-face :inherit 'mode-line :background dark-blue)
      (mode-line-read-only-inactive-face :inherit 'mode-line-inactive :foreground dark-blue)
      (mode-line-read-write-face :inherit 'mode-line :background red)
      (mode-line-read-write-inactive-face :inherit 'mode-line-inactive :foreground red)
      (mode-line-filename-face :inherit 'mode-line :foreground orange :weight 'bold)
      (mode-line-process-face :inherit 'mode-line :foreground green)
      (mode-line-80col-face :inherit 'mode-line :foreground base0 :background yellow)
      ;; vertical border
      (vertical-border :background (doom-lighten modeline-bg 0.2) :foreground (doom-lighten modeline-bg 0.2))
      ;; highlight
      (highlight-thing :background (doom-lighten bg 0.1))
      (show-paren-match :background (doom-lighten bg-alt 0.3)))))

(use-package jp-look
  :config
  (progn
    (setq jp-default-font "Iosevka Extralight-14")
    (setq jp-variable-pitch-font "Lucida Grande-14")
    (setq jp-fixed-pitch-font "Iosevka Extralight-14")))

(use-package all-the-icons
  :straight t)

(use-package moody
  :straight t
  :config
  (progn
    (setq x-underline-at-descent-line t)
    (setq moody-slant-function #'moody-slant-apple-rgb)))

(use-package jp-modeline)

(provide 'jp-ui)
;;; jp-ui.el ends here
