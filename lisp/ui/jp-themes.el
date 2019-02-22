;;; jp-themes.el --- doom themes helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'doom-themes)

(defvar jp-current-theme-dark-p t)
(defvar jp-light-theme 'doom-one-light)
(defvar jp-dark-theme 'doom-one)

(defmacro jp-themes--set-faces (theme-name &rest faces)
  (declare (indent 1) (doc-string 3))
  `(custom-theme-set-faces
    ,theme-name
    ,@(mapcar #'doom-themes--build-face faces)))

(defun jp-themes--get-current ()
  (if jp-current-theme-dark-p jp-dark-theme jp-light-theme))

(defun jp-darken-or-lighten (color alpha)
  (if jp-current-theme-dark-p
      (doom-lighten color alpha)
    (doom-darken color alpha)))

(defun jp-themes--customize-faces ()
  ;; Customize faces
  (jp-themes--set-faces (jp-themes--get-current)
    ;; mode-line
    (mode-line :background modeline-bg
               :foreground modeline-fg
               :overline (jp-darken-or-lighten modeline-bg 0.2)
               :underline (jp-darken-or-lighten modeline-bg 0.2))
    (mode-line-inactive :background modeline-bg-inactive
                        :foreground modeline-fg-alt
                        :overline (jp-darken-or-lighten modeline-bg 0.2)
                        :underline (jp-darken-or-lighten modeline-bg 0.2))
    (mode-line-read-only-face :inherit 'mode-line :background dark-blue :foreground bg-alt)
    (mode-line-read-only-inactive-face :inherit 'mode-line-inactive :foreground dark-blue)
    (mode-line-read-write-face :inherit 'mode-line :background magenta :foreground bg-alt)
    (mode-line-read-write-inactive-face :inherit 'mode-line-inactive :foreground magenta)
    (mode-line-filename-face :inherit 'mode-line :foreground blue :weight 'bold)
    (mode-line-process-face :inherit 'mode-line :foreground green)
    (mode-line-80col-face :inherit 'mode-line :foreground base0 :background yellow)
    ;; vertical border
    (vertical-border :background (jp-darken-or-lighten modeline-bg 0.2)
                     :foreground (jp-darken-or-lighten modeline-bg 0.2))
    ;; highlight
    (symbol-overlay-default-face :background (jp-darken-or-lighten bg 0.1))
    (symbol-overlay-face-1 :background blue :foreground bg-alt)
    (symbol-overlay-face-2 :background orange :foreground bg-alt)
    (symbol-overlay-face-3 :background yellow :foreground bg-alt)
    (symbol-overlay-face-4 :background teal :foreground bg-alt)
    (symbol-overlay-face-5 :background red :foreground bg-alt)
    (symbol-overlay-face-6 :background magenta :foreground bg-alt)
    (symbol-overlay-face-7 :background violet :foreground bg-alt)
    (symbol-overlay-face-8 :background cyan :foreground bg-alt)
    ;; parens
    (show-paren-match :background (jp-darken-or-lighten bg-alt 0.3))
    ;; s-exp
    (hl-sexp-face :background (jp-darken-or-lighten (doom-color 'bg) 0.03))))

(defun jp-themes-load ()
  (load-theme (jp-themes--get-current) t)
  (doom-themes-org-config)
  (jp-themes--customize-faces))

(defun jp-themes-toggle-light-dark ()
  (interactive)
  (setq jp-current-theme-dark-p (not jp-current-theme-dark-p))
  (jp-themes-load))

(provide 'jp-themes)

;;; jp-themes.el ends here
