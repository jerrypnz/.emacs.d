;;; jp-look.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'dash)

(defvar jp-default-font nil
  "The universal default font.")

(defvar jp-variable-pitch-font nil
  "The font to use in the variable-pitch face.")

(defvar jp-fixed-pitch-font nil
  "The font to use in the fixed-pitch face.")

;; Work around Emacs frame sizing bug when line-spacing
;; is non-zero, which impacts e.g. grizzl, and allow resizing when
;; vertical modes are enabled or user has customized jp-resize-minibuffer
(defvar jp-resize-minibuffer nil
  "Whether the minibuffer should be resizable.")

(defun jp-resize-minibuffer-p ()
  (or (-any? 'featurep '(ivy grizzl ido-vertical-mode))
      jp-resize-minibuffer))

(defun jp-minibuffer-setup-hook ()
  (if (jp-resize-minibuffer-p)
      (set (make-local-variable 'line-spacing) 0)
    (setq resize-mini-windows nil)))

(add-hook 'minibuffer-setup-hook
          'jp-minibuffer-setup-hook)

(add-hook 'ido-minibuffer-setup-hook
          'jp-minibuffer-setup-hook)

(mapc (lambda (mode)
        (when (fboundp mode) (funcall mode -1)))
      '(scroll-bar-mode tool-bar-mode blink-cursor-mode))

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . 'nil))
  (setq ns-use-thin-smoothing t)
  (setq frame-title-format ""))

(defvar jp-geometry-file
  (expand-file-name ".jp-geometry" user-emacs-directory)
  "The file where frame geometry settings are saved.")

(defun jp-load-frame-geometry ()
  "Load saved frame geometry settings."
  (if (file-readable-p jp-geometry-file)
      (with-temp-buffer
        (insert-file-contents jp-geometry-file)
        (read (buffer-string)))
    '(100 40 0 0)))

(defun jp-save-frame-geometry ()
  "Save current frame geometry settings."
  (with-temp-file jp-geometry-file
    (print (jp-get-geometry) (current-buffer))))

(defun jp-get-geometry ()
  "Get the current geometry of the active frame."
  (list (frame-width) (frame-height) (frame-parameter nil 'top) (frame-parameter nil 'left)))

(defun jp-set-geometry ()
  "Set the default frame geometry using the values loaded from jp-geometry-file."
  (let ((geom (jp-load-frame-geometry)))
    (let ((f-width (nth 0 geom))
          (f-height (nth 1 geom))
          (f-top (nth 2 geom))
          (f-left (nth 3 geom)))
      (setq default-frame-alist
            (append default-frame-alist
                    `((width . ,f-width)
                      (height . ,f-height)
                      (top . ,f-top)
                      (left . ,f-left)))))))

(defun jp-maybe-set-default-font (default-font var-pitch-font pitch-font)
  "Set up default fonts when they are not set."
  (unless jp-default-font
    (setq jp-default-font default-font))
  (unless jp-variable-pitch-font
    (setq jp-variable-pitch-font var-pitch-font))
  (unless jp-fixed-pitch-font
    (setq jp-fixed-pitch-font pitch-font)))

(defun jp-set-fonts ()
  "Set up default fonts."
  (cond
   ((eq system-type 'darwin)
    (jp-maybe-set-default-font "Monaco-11"
                               "Lucida Grande-11"
                               "Monaco-11"))
   ((eq system-type 'gnu/linux)
    (jp-maybe-set-default-font "DejaVu Sans Mono-10"
                               "Liberation Sans-10"
                               "DejaVu Sans Mono-10"))
   (t
    (jp-maybe-set-default-font (face-font 'default)
                               (face-font 'variable-pitch)
                               (face-font 'fixed-pitch)))))

(defun jp-apply-fonts ()
  (set-face-font 'default jp-default-font)
  (set-face-font 'variable-pitch jp-variable-pitch-font)
  (set-face-font 'fixed-pitch jp-fixed-pitch-font))

(defun jp-look-startup-after-init ()
  "Load defaults for the overall look -- to be called after
loading the init file so as to pick up custom settings."
  (if window-system
      (progn
        (jp-set-geometry)
        (add-hook 'kill-emacs-hook 'jp-save-frame-geometry)
        (setq-default line-spacing 2)
        (jp-set-fonts)
        (jp-apply-fonts)
        (add-to-list 'default-frame-alist `(font . ,jp-default-font))
        (add-to-list 'default-frame-alist '(internal-border-width . 5))
        (set-fringe-mode 5))
    (when (not (eq system-type 'darwin))
      (menu-bar-mode -1))
    ;; Menu bar always off in text mode
    (menu-bar-mode -1)))

(add-hook 'after-init-hook 'jp-look-startup-after-init)

(provide 'jp-look)
;;; jp-look.el ends here
