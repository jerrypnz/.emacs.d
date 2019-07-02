;;; jp-basic-settings.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

;; Make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; better defaults
(setq save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      save-place-file (concat user-emacs-directory "places"))

;; Don't show the startup message
(setq inhibit-startup-message t)

;; Save backup files in the temporary directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Shorten yes/no answers to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable 'power user' features
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; OS X stuff
;;; I prefer cmd key for meta
(setq mac-command-modifier 'meta
      mac-option-modifier 'super)

;; Smooth scroll
(setq scroll-step            1
      scroll-conservatively  10000
      scroll-margin          5
      scroll-preserve-screen-position t)

;; No ring bell at all.
(setq ring-bell-function 'ignore)

;; Window management
(setq split-height-threshold nil)         ; Prefer splitting windows horizontally
(setq split-width-threshold 200)

;; Don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Better scrolling with mouse wheel/trackpad.
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))

;; Character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Automatically update buffers when files change
(global-auto-revert-mode t)
;; apply syntax highlighting to all buffers
(global-font-lock-mode t)
;; Delete marked text on typing
(delete-selection-mode t)
;; Soft-wrap lines
;;(global-visual-line-mode t)

(provide 'jp-basic-settings)

;;; jp-basic-settings.el ends here
