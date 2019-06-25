;;; jp-base.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns x))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (add-to-list 'exec-path-from-shell-variables "GOPATH")
    (exec-path-from-shell-initialize)))

(use-package jp-env)
(use-package jp-programming)

;; some useful global commands
(use-package jp-commands
  :commands (xml-pretty-print rename-file-and-buffer macroexpand-point)
  :bind (("C-x k"   . kill-default-buffer)
         ("C-x C-k" . kill-buffer-and-window)
         ("C-c n"   . create-new-buffer)
         ("C-;"     . insert-semicolon-at-end-of-line)
         ("C-M-;"   . comment-current-line-dwim)
         ("C-o"     . jp-newline)
         ("M-j"     . join-next-line)))

;; expand-region
(use-package expand-region
  :straight t
  :bind
  (("C-=" . er/expand-region)
   ("C--" . er/contract-region)))

;; smex
(use-package smex
  :straight t
  :defer t
  :init (setq-default smex-history-length 32))

;; rainbow delimiters
(use-package rainbow-delimiters
  :straight t)

;; highlight-sexp
(use-package highlight-sexp
  :straight (:host github :type git :repo "jerrypnz/highlight-sexp")
  :config
  (progn
    (add-hook 'lisp-mode-hook 'highlight-sexp-mode)
    (add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)
    (add-hook 'clojure-mode-hook 'highlight-sexp-mode)))

;; undo-tree
(use-package undo-tree
  :straight t
  :config
  (progn
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-visualizer-timestamps t)
    (global-undo-tree-mode)))

(use-package hydra
  :straight t
  :config
  (progn
    (setq hydra-hint-display-type 'posframe)
    (setq hydra-posframe-show-params
          '(:internal-border-width 20
            :background-color "#242933"
            :override-parameters ((alpha 100 100))
            :poshandler (lambda (info)
                          (cons (car (posframe-poshandler-frame-center info))
                                (- (cdr (posframe-poshandler-frame-bottom-left-corner info)) 10)))))))

;; Major mode keys managed by a pretty hydra
(use-package major-mode-hydra
  :straight (:host github :type git :repo "jerrypnz/major-mode-hydra.el" :branch "develop")
  :demand t
  :bind
  (("C-M-SPC" . major-mode-hydra))

  :config
  (progn
    (setq major-mode-hydra-invisible-quit-key "q")
    (defun jp-major-mode-hydra-title-generator (_)
      `(with-mode-icon major-mode
                       (propertize (s-concat (format-mode-line mode-name) " Commands")
                                   'face '(:weight bold :height 1.1))
                       1.1))

    (setq major-mode-hydra-title-generator #'jp-major-mode-hydra-title-generator)))

(use-package jp-main-hydra
  :bind
  ("M-SPC" . jp-main-hydra/body))

;; mwim
(use-package mwim
  :straight t
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)))

(use-package easy-kill
  :straight t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill))

;; iedit
(use-package iedit
  :straight t
  :init
  (global-unset-key (kbd "M-r"))
  (setq iedit-toggle-key-default (kbd "M-r")))

;; dump-jump
(use-package dumb-jump
  :straight t
  :bind
  (("M-."   . dumb-jump-go)
   ("C-M-." . dumb-jump-go-other-window))
  :config
  (progn
    (setq dumb-jump-selector 'ivy)
    (setq dumb-jump-prefer-searcher 'rg)))

;; symbol-overlay
(use-package symbol-overlay
  :straight t
  :defer nil
  :bind
  (("M-s" . symbol-overlay-put))
  :config
  (progn
    ;; TODO add a hydra
    ;;(setq symbol-overlay-map nil)
    (setq symbol-overlay-idle-time 0.1)
    (add-hook 'jp-prog-mode-hook #'symbol-overlay-mode)
    (eval-after-load "swiper"
      '(defadvice swiper (before jp-swiper-remove-highlight activate)
         ;; If the search string happens to be the symbol being
         ;; highlighted by `highlight-thing', the overlays it applies
         ;; should be removed, because `swiper' applies its own
         ;; overlays. Otherwise it can flicker between the two faces
         ;; as you move between candidates.
         (symbol-overlay-remove-temp)))))

;; rectangle
(use-package jp-rect
  :bind
  ("C-x r" . jp-rectangle/body))

;; avy
(use-package avy
  :straight t
  :defer nil
  :bind
  (("M-g c" . avy-goto-char)
   ("M-g g" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("C-M-g" . avy-goto-word-1))
  :config
  (setq avy-background t))

(use-package frog-menu
  :straight t
  :defer t
  :config
  (progn
    (setf (alist-get 'avy-posframe frog-menu-display-option-alist)
          'posframe-poshandler-window-center)
    (setq frog-menu-avy-padding t)
    (setq frog-menu-posframe-parameters
          '((internal-border-width . 10)
            (border-width . 2)))))

;; frog-jump-buffer
(use-package frog-jump-buffer
  :straight t
  :bind (("M-i" . frog-jump-buffer))
  :config
  (progn
    (setq frog-jump-buffer-default-filter 'frog-jump-buffer-filter-same-project
          frog-jump-buffer-current-filter-function 'frog-jump-buffer-filter-same-project
          frog-jump-buffer-include-current-buffer nil)

    (defun jp-frog-jump-buffer (f &rest args)
      (let* ((avy-background nil)) ;; no background dimming
        (apply f args)))

    (advice-add #'frog-jump-buffer :around #'jp-frog-jump-buffer)))

;; ace-window
(use-package ace-window
  :straight t
  :bind
  (("M-o" . ace-window))
  :config
  (progn
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))
    (setq aw-background t)
    (setq aw-minibuffer-flag t)))

(use-package move-border
  :straight (:host github :repo "ramnes/move-border" :branch "master"))

(use-package zoom-frm
  :straight (:host github :repo "emacsmirror/zoom-frm" :branch "master"))

;; window management hydra
(use-package jp-window
  :after (move-border zoom-frm)
  :bind
  ("C-M-o" . jp-window/body))

;; ispell
(use-package ispell
  :config
  (progn
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t)))

;; TODO configuration
(use-package imenu-list
  :straight t)

(use-package helpful
  :straight t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-at-point
             helpful-command))

(use-package jp-help
  :init (global-set-key (kbd "C-h") 'jp-help/body)
  :commands (jp-help/body))

(use-package page-break-lines
  :straight t
  :commands (page-break-lines-mode))

(use-package whitespace-cleanup-mode
  :straight t
  :commands (whitespace-cleanup-mode))

(use-package rainbow-mode
  :straight t
  :commands (rainbow-mode))

(use-package deadgrep
  :straight t
  :commands (deadgrep))

(provide 'jp-base)
;;; jp-base.el ends here
