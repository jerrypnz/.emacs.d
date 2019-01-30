;;; jp-ivy.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

;; ivy
(use-package ivy
  :straight t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")))

(use-package ivy-hydra
  :straight t)

(use-package wgrep
  :straight t)

(use-package counsel
  :straight t
  :bind
  (("M-x"     . counsel-M-x)
   ("M-y"     . counsel-yank-pop)
   ("C-s"     . counsel-grep-or-swiper)
   ("C-x C-f" . counsel-find-file))

  :config
  (progn
    (setq counsel-grep-base-command
          "rg -i -M 120 --no-heading --line-number --color never '%s' %s")))

(use-package jp-ivy-utils
  :bind
  ("C-M-s" . jp-counsel-grep-or-swiper-symbol-at-pt))

;; (use-package ivy-posframe
;;   :straight t
;;   :config
;;   (progn
;;     (push '(t . ivy-posframe-display-at-frame-center) ivy-display-functions-alist)
;;     (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
;;     (setq ivy-posframe-hide-minibuffer t)
;;     (setq ivy-posframe-parameters
;;           '((no-accept-focus . t)
;;             (no-focus-on-map . t)
;;             (internal-border-width . 8)
;;             (width  . 120)
;;             (min-height  . 12)
;;             (vertical-scroll-bars . nil)
;;             (horizontal-scroll-bars . nil)
;;             (left-fringe . 10)
;;             (right-fringe . 10)
;;             (menu-bar-lines . 0)
;;             (tool-bar-lines . 0)
;;             (line-spacing . 0)
;;             (unsplittable . t)
;;             (top . 100)
;;             (mouse-wheel-frame . nil)
;;             (no-other-frame . t)
;;             (cursor-type . nil)
;;             (drag-internal-border . t)
;;             (no-special-glyphs . t))))
;;   (ivy-posframe-enable))

(provide 'jp-ivy)
;;; jp-ivy.el ends here
