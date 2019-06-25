;;; jp-themes.el --- doom themes helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'doom-themes)

(defvar jp-current-theme-dark-p t)
(defvar jp-light-theme 'doom-one-light)
(defvar jp-dark-theme 'doom-one)

(defun jp-themes--get-current ()
  (if jp-current-theme-dark-p jp-dark-theme jp-light-theme))

(defun jp-themes--customize-faces ()
  ;; Customize faces
  (let ((theme (jp-themes--get-current))
        (color-fn (if jp-current-theme-dark-p 'doom-lighten 'doom-darken))
        (status-fg (if jp-current-theme-dark-p 'fg 'bg)))
    (doom-themes-set-faces theme
      ;; mode-line
      `(mode-line :background modeline-bg
                  :foreground modeline-fg
                  :overline (,color-fn modeline-bg 0.2)
                  :underline (,color-fn modeline-bg 0.2))
      `(mode-line-inactive :background modeline-bg-inactive
                           :foreground modeline-fg-alt
                           :overline (,color-fn modeline-bg 0.2)
                           :underline (,color-fn modeline-bg 0.2))
      `(mode-line-read-only-face :inherit 'mode-line :background dark-blue :foreground ,status-fg)
      '(mode-line-read-only-inactive-face :inherit 'mode-line-inactive :foreground dark-blue)
      `(mode-line-read-write-face :inherit 'mode-line :background red :foreground ,status-fg)
      '(mode-line-read-write-inactive-face :inherit 'mode-line-inactive :foreground red)
      '(mode-line-filename-face :inherit 'mode-line :foreground blue :weight 'bold)
      '(mode-line-process-face :inherit 'mode-line :foreground green)
      '(mode-line-80col-face :inherit 'mode-line :foreground base0 :background yellow)
      ;; vertical border
      `(vertical-border :background (,color-fn modeline-bg 0.2)
                        :foreground (,color-fn modeline-bg 0.2))
      ;; highlight
      `(symbol-overlay-default-face :background (,color-fn bg 0.1))
      '(symbol-overlay-face-1 :background blue :foreground bg-alt)
      '(symbol-overlay-face-2 :background orange :foreground bg-alt)
      '(symbol-overlay-face-3 :background yellow :foreground bg-alt)
      '(symbol-overlay-face-4 :background teal :foreground bg-alt)
      '(symbol-overlay-face-5 :background red :foreground bg-alt)
      '(symbol-overlay-face-6 :background magenta :foreground bg-alt)
      '(symbol-overlay-face-7 :background violet :foreground bg-alt)
      '(symbol-overlay-face-8 :background cyan :foreground bg-alt)
      ;; parens
      `(show-paren-match :background (,color-fn bg-alt 0.3))
      ;; s-exp
      `(hl-sexp-face :background (,color-fn bg 0.03))
      ;; frog-menu
      `(frog-menu-border :background (,color-fn modeline-bg 0.2))
      `(frog-menu-posframe-background-face :background bg-alt)
      '(frog-menu-prompt-face :foreground fg-alt)
      '(frog-menu-candidates-face :foreground fg)
      '(frog-menu-actions-face :foreground blue)
      '(frog-menu-action-keybinding-face :foreground orange))))

(defun jp-themes-load ()
  (load-theme (jp-themes--get-current) t)
  (doom-themes-org-config)
  (doom-themes-treemacs-config)
  (jp-themes--customize-faces))

(defun jp-themes-toggle-light-dark ()
  (interactive)
  (setq jp-current-theme-dark-p (not jp-current-theme-dark-p))
  (jp-themes-load))

(provide 'jp-themes)

;;; jp-themes.el ends here
