;;; jp-themes.el --- doom themes helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'doom-themes)

(defvar jp-current-theme-dark-p t)
(defvar jp-light-theme 'doom-one-light)
(defvar jp-dark-theme 'doom-one)
(defvar jp-default-font nil)
(defvar jp-variable-pitch-font nil)

(defun jp-themes--get-current ()
  (if jp-current-theme-dark-p jp-dark-theme jp-light-theme))

(defun jp-themes--set-font ()
  (when jp-default-font
    (set-face-font 'default jp-default-font)
    (set-face-font 'fixed-pitch jp-default-font))
  (when jp-variable-pitch-font
    (set-face-font 'variable-pitch jp-variable-pitch-font)))

(defun jp-themes--customize-faces ()
  ;; Customize faces
  (let ((theme (jp-themes--get-current))
        (color-fn (if jp-current-theme-dark-p 'doom-lighten 'doom-darken))
        (status-fg (if jp-current-theme-dark-p 'fg 'bg)))
    (doom-themes-set-faces theme
      `(fringe :inherit 'default)
      ;; header-line
      `(header-line :background bg
                    :foreground fg
                    :underline fg
                    :box nil)
      ;; mode-line
      `(mode-line :background bg
                  :foreground bg
                  :height 10
                  :underline fg)
      `(mode-line-inactive :background bg
                           :foreground bg
                           :height 10
                           :underline fg)
      ;; mode-line
      ;; `(mode-line :background modeline-bg;
      ;;             :foreground modeline-fg
      ;;             :overline (,color-fn modeline-bg 0.2)
      ;;             :underline (,color-fn modeline-bg 0.2))
      ;; `(mode-line-inactive :background modeline-bg
      ;;                      :foreground modeline-fg-alt
      ;;                      :overline (,color-fn modeline-bg 0.2)
      ;;                      :underline (,color-fn modeline-bg 0.2))
      `(mode-line-read-only-face :inherit 'mode-line :background dark-blue :foreground ,status-fg)
      '(mode-line-read-only-inactive-face :inherit 'mode-line-inactive :foreground dark-blue)
      `(mode-line-read-write-face :inherit 'mode-line :background red :foreground ,status-fg)
      '(mode-line-read-write-inactive-face :inherit 'mode-line-inactive :foreground red)
      '(mode-line-filename-face :inherit 'mode-line :foreground blue :weight 'bold)
      '(mode-line-process-face :inherit 'mode-line :foreground green)
      '(mode-line-80col-face :inherit 'mode-line :foreground base0 :background yellow)
      '(mode-line-success-face :inherit 'mode-line :foreground green)
      '(mode-line-warning-face :inherit 'mode-line :foreground yellow)
      '(mode-line-error-face :inherit 'mode-line :foreground red)
      '(mode-line-info-face :inherit 'mode-line :foreground blue)
      ;; vertical border
      `(vertical-border :background fg
                        :foreground fg)
      ;; diff-hl
      `(diff-hl-insert :foreground green :background (doom-darken green 0.4))
      `(diff-hl-delete :foreground red :background (doom-darken red 0.4))
      `(diff-hl-change :foreground blue :background (doom-darken blue 0.4))
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
      ;; ivy
      '(ivy-posframe :inherit 'default :background modeline-bg)
      `(ivy-posframe-border :inherit 'default :background (,color-fn modeline-bg 0.2))
      ;; hydra-posframe
      '(hydra-posframe-face :inherit 'default :background modeline-bg)
      `(hydra-posframe-border-face :inherit 'default :background (,color-fn modeline-bg 0.2))
      ;; s-exp
      `(hl-sexp-face :background (,color-fn bg 0.03))
      ;; frog-menu
      `(frog-menu-border :background (,color-fn modeline-bg 0.2))
      `(frog-menu-posframe-background-face :background bg-alt)
      '(frog-menu-prompt-face :foreground fg-alt)
      '(frog-menu-candidates-fqace :foreground fg)
      '(frog-menu-actions-face :foreground blue)
      '(frog-menu-action-keybinding-face :foreground orange))))

(defun jp-themes-load ()
  (load-theme (jp-themes--get-current) t nil)
  (jp-themes--set-font)
  (jp-themes--customize-faces)
  (enable-theme (jp-themes--get-current)))

(defun jp-themes-toggle-light-dark ()
  (interactive)
  (setq jp-current-theme-dark-p (not jp-current-theme-dark-p))
  (jp-themes-load))

(provide 'jp-themes)

;;; jp-themes.el ends here
