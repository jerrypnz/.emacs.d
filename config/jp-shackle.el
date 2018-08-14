;;; jp-shackle.el --- Better popup window management with shackle -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package shackle
  :straight t
  :config
  (progn
    (setq shackle-rules
          '(("*Help*" :select t :align right :size 0.33)))
    (setq shackle-select-reused-windows t)
    (shackle-mode)))

(provide 'jp-shackle.el)
