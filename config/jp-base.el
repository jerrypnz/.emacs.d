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
    ;(setq exec-path-from-shell-shell-name "/usr/local/bin/zsh")
    ;(setq exec-path-from-shell-arguments '("-l"))
    (setq exec-path-from-shell-variables (append exec-path-from-shell-variables
                                                 '("GOPATH"
                                                   "JAVA_OPTS"
                                                   "HTTP_PROXY"
                                                   "HTTPS_PROXY")))
    (exec-path-from-shell-initialize)))

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

;; amx
(use-package amx
  :straight t
  :init (setq amx-history-length 32))

;; highlight-sexp
(use-package highlight-sexp
  :straight (:host github :type git :repo "jerrypnz/highlight-sexp")
  :hook
  ((lisp-mode emacs-lisp clojure-mode) . highlight-sexp-mode))

;; highlight todo
(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces (--filter (member (car it) '("TODO" "FIXME" "NOTE"))
                                        hl-todo-keyword-faces)))

;; undo-tree
(use-package undo-tree
  :straight t
  :config
  (progn
    (setq undo-tree-history-directory-alist `(("." . ,jp-emacs-undo-history-dir)))
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-visualizer-timestamps t)
    (global-undo-tree-mode)))

(use-package hydra
  :straight t)

(use-package posframe
  :straight t
  :config
  (defun jp-frame-bottom-poshandler (info)
    (let ((pos (posframe-poshandler-frame-center info)))
      (cons (car pos) (+ (cdr pos) 100)))))

(use-package hydra-posframe
  :straight (:host github :repo "Ladicle/hydra-posframe"
             :fork (:host github :repo "jerrypnz/hydra-posframe"))
  :hook (after-init . hydra-posframe-enable)
  :config
  (setq hydra-posframe-poshandler 'jp-frame-bottom-poshandler)
  (setq hydra-posframe-parameters '((alpha 100 100)
                                    (left-fringe . 10)
                                    (right-fringe . 10))))

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
  (setq dumb-jump-prefer-searcher 'rg))

;; symbol-overlay
(use-package symbol-overlay
  :straight t

  :hook
  ((prog-mode
    css-mode
    sgml-mode
    html-mode) . symbol-overlay-mode)

  :bind
  (("M-s" . symbol-overlay-put))

  :config
  (setq symbol-overlay-idle-time 0.1))

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
   ("C-M-g" . avy-goto-char-timer))
  :config
  (setq avy-background t))

;; (use-package frog-menu
;;   :straight t
;;   :defer t
;;   :config
;;   (progn
;;     (setf (alist-get 'avy-posframe frog-menu-display-option-alist)
;;           'posframe-poshandler-window-center)
;;     (setq frog-menu-avy-padding t)
;;     (setq frog-menu-posframe-parameters
;;           '((internal-border-width . 1)
;;             (left-fringe . 10)
;;             (right-fringe . 10)))))

;; frog-jump-buffer
;; (use-package frog-jump-buffer
;;   :straight t
;;   :bind (("M-i" . frog-jump-buffer))
;;   :config
;;   (progn
;;     (setq frog-jump-buffer-default-filter 'frog-jump-buffer-filter-same-project
;;           frog-jump-buffer-current-filter-function 'frog-jump-buffer-filter-same-project
;;           frog-jump-buffer-include-current-buffer nil)

;;     (defun jp-frog-jump-buffer (f &rest args)
;;       (let* ((avy-background nil)) ;; no background dimming
;;         (apply f args)))

;;     (advice-add #'frog-jump-buffer :around #'jp-frog-jump-buffer)))

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

(use-package helpful
  :straight t

  :pretty-hydra
  ((:color teal :quit-key "q")
   ("Helpful"
    (("f" helpful-callable "callable")
     ("v" helpful-variable "variable")
     ("k" helpful-key "key")
     ("c" helpful-command "command")
     ("d" helpful-at-point "thing at point"))))

  :bind ("C-h" . helpful-hydra/body))

(use-package page-break-lines
  :straight t
  :hook (prog-mode . page-break-lines-mode))

(use-package whitespace-cleanup-mode
  :straight t
  :config (global-whitespace-cleanup-mode))

(use-package editorconfig
  :straight t
  :config (editorconfig-mode 1))

(use-package rainbow-mode
  :straight t
  :commands (rainbow-mode))

(use-package deadgrep
  :straight t
  :commands (deadgrep))

(use-package dash-at-point
  :straight t

  :config
  (add-to-list 'dash-at-point-mode-alist '(clojure-mode . "clojure,java,javadoc,cljdoc"))

  :pretty-hydra
  ((:color teal :quit-key "q")
   ("Dash"
    (("d" dash-at-point "at point")
     ("e" dash-at-point-with-docset "with docset")))))

(provide 'jp-base)
;;; jp-base.el ends here
