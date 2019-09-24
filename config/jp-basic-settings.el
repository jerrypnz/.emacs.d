;;; jp-basic-settings.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; remove scroll bar, tool bar, blinking cursor
(mapc (lambda (mode)
        (when (fboundp mode) (funcall mode -1)))
      '(scroll-bar-mode tool-bar-mode menu-bar-mode blink-cursor-mode))

(setq
 ;; better defaults
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t
 require-final-newline t
 visible-bell t
 load-prefer-newer t
 ;; no startup screen
 inhibit-startup-message t
 ;; save backup files in the temporary directory
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 ;; smooth scrolling
 scroll-step            1
 scroll-conservatively  10000
 scroll-margin          5
 scroll-preserve-screen-position t
 ;; no ring bell
 ring-bell-function 'ignore
 ;; prefer splitting windows horizontally
 split-height-threshold nil
 split-width-threshold 200)

;; shorten yes/no answers to y/n
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default
 ;; line spacing
 line-spacing 0
 ;; don't use tabs for indent; replace tabs with two spaces.
 tab-width 2
 indent-tabs-mode nil)

;; enable 'power user' features
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; better scrolling with mouse wheel/trackpad.
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))

;; character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; automatically update buffers when files change
(global-auto-revert-mode t)
;; apply syntax highlighting to all buffers
(global-font-lock-mode t)
;; delete marked text on typing
(delete-selection-mode t)
;; save place
(save-place-mode t)
;; soft-wrap lines
;;(global-visual-line-mode t)

(when window-system
  ;; os x stuff
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'nil
          icon-title-format nil
          frame-title-format nil)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    ;; ligature support
    (when (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode t)))

  ;; window divier
  (setq
   window-divider-default-places 'right-only
   window-divider-default-right-width 1)
  (window-divider-mode 1)

  ;; set fringe width
  (set-fringe-mode 5))


;; Configuration for Emacs builtin packages


;; Make buffer names unique
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package hippie-expand
  :bind ("M-/" . hippie-expand))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package apropos
  :defer t
  :config
  (setq apropos-do-all t))

;; ispell
(use-package ispell
  :config
  (progn
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t)))

;; ediff
(use-package ediff
  :defer t
  :config
  (progn
    (defvar jp-ediff-last-windows nil)

    (defun jp-store-pre-ediff-winconfig ()
      (setq jp-ediff-last-windows (current-window-configuration)))

    (defun jp-restore-pre-ediff-winconfig ()
      (set-window-configuration jp-ediff-last-windows))

    (setq ediff-window-setup-function 'ediff-setup-windows-plain)
    (add-hook 'ediff-before-setup-hook #'jp-store-pre-ediff-winconfig)
    (add-hook 'ediff-quit-hook #'jp-restore-pre-ediff-winconfig)))

(provide 'jp-basic-settings)

;;; jp-basic-settings.el ends here
