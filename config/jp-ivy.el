;;; jp-ivy.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; ivy
(use-package ivy
  :straight t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-height 20)))

(use-package ivy-hydra
  :straight t)

(use-package wgrep
  :straight t)

(use-package counsel
  :straight t
  :pretty-hydra
  (helpful-hydra
   (""
    (("F" counsel-faces "faces")
     ("a" counsel-apropos "apropos")
     ("i" counsel-info-lookup-symbol "info lookup"))))

  :bind
  (("M-x"     . counsel-M-x)
   ("M-y"     . counsel-yank-pop)
   ("C-s"     . counsel-grep-or-swiper)
   ("C-x C-f" . counsel-find-file))

  :config
  (progn
    (setq counsel-grep-base-command
          "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
    (setq counsel-outline-face-style 'org)))

(use-package ivy-rich
  :straight t
  :config (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :straight t
  :after (ivy-rich counsel-projectile)
  :config
  (setq all-the-icons-ivy-rich-icon-size 0.8)
  (plist-put all-the-icons-ivy-rich-display-transformers-list
             'counsel-projectile-switch-to-buffer
             '(:columns
               ((all-the-icons-ivy-rich-buffer-icon)
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning)))
               :delimiter "\t"))
  (all-the-icons-ivy-rich-mode 1))

(use-package jp-ivy-utils
  :bind
  ("C-M-s" . jp-counsel-grep-or-swiper-symbol-at-pt))

(use-package ivy-posframe
  :straight t
  :config
  (progn
    (defun ivy-posframe-display--jp-custom (str)
      (ivy-posframe--display str #'jp-frame-bottom-poshandler))

    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display--jp-custom))
          ivy-posframe-border-width 1
          ivy-posframe-hide-minibuffer t
          ivy-posframe-min-width 100
          ivy-posframe-min-height 10
          ivy-posframe-parameters '((alpha 100 100)
                                    (max-width . 140)
                                    (left-fringe . 10)
                                    (right-fringe . 10)))

    (ivy-posframe-mode 1)))

(provide 'jp-ivy)
;;; jp-ivy.el ends here
