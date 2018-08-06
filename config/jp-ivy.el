;;; jp-ivy.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

;; ivy
(use-package ivy
  :straight t
  :bind
  ("M-m r" . ivy-resume)

  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")))

(use-package all-the-icons-ivy
  :straight t
  :config
  (progn
    (setq all-the-icons-ivy-file-commands
          '(counsel-find-file
            counsel-file-jump
            counsel-recentf
            counsel-projectile
            counsel-projectile-find-file
            counsel-projectile-find-dir))
    (all-the-icons-ivy-setup)))

;; (use-package ivy-posframe
;;   :straight t
;;   :config
;;   (progn
;;     (push '(t . ivy-posframe-display-at-frame-center) ivy-display-functions-alist)
;;     (setq ivy-posframe-parameters
;;           '((no-accept-focus . t)
;;             (no-focus-on-map . t)
;;             (internal-border-width . 8)
;;             (width  . 90)
;;             (min-height  . 12)
;;             (vertical-scroll-bars . nil)
;;             (horizontal-scroll-bars . nil)
;;             (left-fringe . 0)
;;             (right-fringe . 0)
;;             (menu-bar-lines . 0)
;;             (tool-bar-lines . 0)
;;             (line-spacing . 0)
;;             (unsplittable . t)
;;             (top . 100)
;;             (mouse-wheel-frame . nil)
;;             (no-other-frame . t)
;;             (cursor-type . nil)
;;             (drag-internal-border . t)
;;             (left-fringe . 0)
;;             (right-fringe . 0)
;;             (background-color . "#282C34")
;;             (no-special-glyphs . t))))
;;   (ivy-posframe-enable))

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
   ("C-x C-f" . counsel-find-file)
   ("C-h f"   . counsel-describe-function)
   ("C-h v"   . counsel-describe-variable))

  :config
  (progn
    (setq counsel-grep-base-command
          "rg -i -M 120 --no-heading --line-number --color never '%s' %s")))

(use-package jp-ivy-utils
  :bind
  ("C-M-s" . jp-counsel-grep-or-swiper-symbol-at-pt))

(provide 'jp-ivy)
;;; jp-ivy.el ends here
