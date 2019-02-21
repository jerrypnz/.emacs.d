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
    (load-theme 'doom-nord-light t)

    (doom-themes-org-config)

    (defmacro jp-doom-themes-set-faces (theme-name &rest faces)
      (declare (indent 1) (doc-string 3))
      `(custom-theme-set-faces
        ,theme-name
        ,@(mapcar #'doom-themes--build-face faces)))

    ;; Customize faces
    (jp-doom-themes-set-faces 'doom-nord-light
      ;; mode-line
      (mode-line :background modeline-bg
                 :foreground modeline-fg
                 :overline (doom-darken modeline-bg 0.2)
                 :underline (doom-darken modeline-bg 0.2))
      (mode-line-inactive :background modeline-bg-inactive
                          :foreground modeline-fg-alt
                          :overline (doom-darken modeline-bg 0.2)
                          :underline (doom-darken modeline-bg 0.2))
      (mode-line-read-only-face :inherit 'mode-line :background teal :foreground bg-alt)
      (mode-line-read-only-inactive-face :inherit 'mode-line-inactive :foreground teal)
      (mode-line-read-write-face :inherit 'mode-line :background red :foreground bg-alt)
      (mode-line-read-write-inactive-face :inherit 'mode-line-inactive :foreground violet)
      (mode-line-filename-face :inherit 'mode-line :foreground teal :weight 'bold)
      (mode-line-process-face :inherit 'mode-line :foreground green)
      (mode-line-80col-face :inherit 'mode-line :foreground base0 :background yellow)
      ;; vertical border
      (vertical-border :background (doom-darken modeline-bg 0.2) :foreground (doom-darken modeline-bg 0.2))
      ;; highlight
      (symbol-overlay-default-face :background (doom-darken bg 0.1))
      (symbol-overlay-face-1 :background blue :foreground bg-alt)
      (symbol-overlay-face-2 :background orange :foreground bg-alt)
      (symbol-overlay-face-3 :background yellow :foreground bg-alt)
      (symbol-overlay-face-4 :background teal :foreground bg-alt)
      (symbol-overlay-face-5 :background red :foreground bg-alt)
      (symbol-overlay-face-6 :background magenta :foreground bg-alt)
      (symbol-overlay-face-7 :background violet :foreground bg-alt)
      (symbol-overlay-face-8 :background cyan :foreground bg-alt)
      (show-paren-match :background (doom-darken bg-alt 0.3)))))

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
