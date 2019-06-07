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
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-height 20)))

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


(use-package ivy-posframe
  :straight t
  :config
  (progn
    (defun jp-ivy-posframe-display (str)
      (ivy-posframe--display str (lambda (info)
                                   (cons (car (posframe-poshandler-frame-center info))
                                         (- (cdr (posframe-poshandler-frame-bottom-left-corner info)) 10)))))

    (setq ivy-posframe-display-functions-alist '((t . jp-ivy-posframe-display))
          ivy-posframe-border-width 20
          ivy-posframe-hide-minibuffer t
          ivy-posframe-width 80
          ivy-posframe-min-width 100
          ivy-posframe-min-height 10
          ivy-posframe-parameters '((alpha 100 100)
                                    (max-width 120)))
    (ivy-posframe-mode 1)))

(provide 'jp-ivy)
;;; jp-ivy.el ends here
