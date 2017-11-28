;;; jp-base.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (progn
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

(use-package jp-look
  :config
  (progn
    (setq jp-default-font "Inconsolata-13")
    (setq jp-variable-pitch-font "Lucida Grande-12")
    (setq jp-fixed-pitch-font "Inconsolata-13")))

(use-package all-the-icons)

;; themes

;; Ample theme
;;(use-package ample-theme
;;  :config
;;  (progn
;;    (load-theme 'ample t t)
;;    (load-theme 'ample-flat t t)
;;    (load-theme 'ample-light t t)
;;    (enable-theme 'ample-flat)))

;; Doom theme
(use-package doom-themes
  :config
  (progn
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled

    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
    ;; may have their own settings.
    (load-theme 'doom-one t)

    ;; Enable flashing mode-line on errors
    ;;(doom-themes-visual-bell-config)

    ;; Enable custom neotree theme
    ;;(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)

    ;; Nicer colors for org heading level 3 to 6. Colors are taken
    ;; from doom-one theme.
    (eval-after-load "org"
      '(progn
         (set-face-attribute 'org-level-3 nil :foreground "#DA8548")
         (set-face-attribute 'org-level-4 nil :foreground "#ECBE7B")
         (set-face-attribute 'org-level-5 nil :foreground "#ECBE7B")
         (set-face-attribute 'org-level-6 nil :foreground "#ECBE7B")))))

(use-package jp-env)
(use-package jp-programming)
(use-package jp-modeline)

;; some useful global commands
(use-package jp-commands
  :commands (xml-pretty-print rename-file-and-buffer macroexpand-point)
  :bind (("C-x k"   . kill-default-buffer)
         ("C-x C-k" . kill-buffer-and-window)
         ("C-c n"   . create-new-buffer)
         ("C-;"     . insert-semicolon-at-end-of-line)
         ("C-M-;"   . comment-current-line-dwim)
         ("C-o"     . start-newline-after)
         ("M-o"     . start-newline-before)
         ("M-j"     . join-next-line)
         ("M-m t n" . toggle-linum)))

;; expand-region
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C--" . er/contract-region)))

;; smex
(use-package smex
  :defer t
  :init (setq-default smex-history-length 32))

;; aggressive-indent
(use-package aggressive-indent
  :config
  (progn
    (global-aggressive-indent-mode 1)
    (add-to-list 'aggressive-indent-excluded-modes 'go-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'restclient-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)))

;; rainbow delimiters
(use-package rainbow-delimiters)

;; undo-tree
(use-package undo-tree
  :config
  (progn
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-visualizer-timestamps t)
    (global-undo-tree-mode)))

;; which-key
(use-package which-key
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-add-key-based-replacements
    "M-m /" "search"
    "M-m g" "git"
    "M-m l" "layouts"
    "M-m p" "projects"
    "M-m t" "toggles"
    "M-m w" "workspace")
  (which-key-mode))

;; mwim
(use-package mwim
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)))

;; iedit
(use-package iedit
  :init
  (global-unset-key (kbd "M-r"))
  (setq iedit-toggle-key-default (kbd "M-r")))

;; rectangle
(use-package jp-rect
  :bind
  ("C-x r" . jp-rectangle/body))

;; avy
(use-package avy
  :defer nil
  :bind
  (("M-g c" . avy-goto-char)
   ("M-g g" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("C-M-g" . avy-goto-word-1))
  :config
  (setq avy-background t))

;; ace-window
(use-package ace-window
  :bind
  (("M-p" . ace-window))
  :config
  (progn
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))))

;; window management hydra
(use-package jp-window
  :bind
  ("C-M-p" . jp-window/body))

(provide 'jp-base)
;;; jp-base.el ends here
