;;; jp-base.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package all-the-icons
  :straight t)

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
    (load-theme 'doom-spacegrey t)

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

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns x))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (add-to-list 'exec-path-from-shell-variables "GOPATH")
    (exec-path-from-shell-initialize)))

(use-package jp-look
  :config
  (progn
    (setq jp-default-font "Inconsolata-13")
    (setq jp-variable-pitch-font "Lucida Grande-12")
    (setq jp-fixed-pitch-font "Inconsolata-13")))

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
         ("C-o"     . jp-newline)
         ("M-j"     . join-next-line)
         ("M-m t n" . toggle-linum)))

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

;; aggressive-indent
(use-package aggressive-indent
  :straight t
  :config
  (progn
    (global-aggressive-indent-mode 1)
    (add-to-list 'aggressive-indent-excluded-modes 'go-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'restclient-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)))

;; rainbow delimiters
(use-package rainbow-delimiters
  :straight t)

;; highlight-sexp
(use-package highlight-sexp
  :straight t
  :config
  (progn
    (setq hl-sexp-background-color "#22252b")
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

;; which-key
(use-package which-key
  :straight t
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-add-key-based-replacements
    "M-m g" "git"
    "M-m l" "layouts"
    "M-m m" "major-mode-hydra"
    "M-m p" "projects"
    "M-m t" "toggles"
    "M-m w" "workspace")
  (which-key-mode))

;; mwim
(use-package mwim
  :straight t
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)))

;; iedit
(use-package iedit
  :straight t
  :init
  (global-unset-key (kbd "M-r"))
  (setq iedit-toggle-key-default (kbd "M-r")))

;; hightlight-things.el
(use-package highlight-thing
  :straight t
  :config
  (progn
    (setq highlight-thing-case-sensitive-p t)
    (setq highlight-thing-exclude-thing-under-point t)
    (setq highlight-thing-delay-seconds 0.1)
    (set-face-attribute
     'highlight-thing nil
     ;; Using a lighter background color for hightlighted symbols whil
     ;; keeping the same foreground color. Can use this tool to get
     ;; lighter versions of a color:
     ;; https://www.w3schools.com/colors/colors_picker.asp
     :inherit nil
     :foreground nil :background "#383d47")
    (global-highlight-thing-mode)

    (eval-after-load "swiper"
      '(defadvice swiper (before jp-swiper-remove-highlight activate)
         ;; If the search string happens to be the symbol being
         ;; highlighted by `highlight-thing', the overlays it applies
         ;; should be removed, because `swiper' applies its own
         ;; overlays. Otherwise it can flicker between the two faces
         ;; as you move between candidates.
         (highlight-thing-remove-last)))))

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

;; ace-window
(use-package ace-window
  :straight t
  :bind
  (("M-o" . ace-window))
  :config
  (progn
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))
    (setq aw-background nil)))

(use-package move-border
  :straight (:host github :repo "ramnes/move-border" :branch "master"))

(use-package zoom-frm
  :straight (:host github :repo "emacsmirror/zoom-frm" :branch "master"))

;; window management hydra
(use-package jp-window
  :after (move-border zoom-frm)
  :bind
  ("C-M-o" . jp-window/body))

;; Major mode keys managed by a pretty hydra
(use-package major-mode-hydra
  :straight (:host github :repo "jerrypnz/major-mode-hydra.el" :branch "master")
  :init
  (progn
    (autoload 'pretty-hydra-define "pretty-hydra" nil nil 'macro)
    (autoload 'major-mode-hydra-bind "major-mode-hydra" nil 'macro))

  :bind
  (("C-M-m" . major-mode-hydra)
   ("M-m m" . major-mode-hydra)))

;; ispell
(use-package ispell
  :config
  (progn
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t)))

(provide 'jp-base)
;;; jp-base.el ends here
