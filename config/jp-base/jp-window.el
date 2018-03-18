;;; jp-window.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'jp-fancy-hydra)
(require 'ace-window)
(require 'move-border)
(require 'zoom-frm)

(defun jp-zoom-default ()
  "Reset text scale."
  (interactive)
  (zoom-in/out 0))

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-windows.el
(defun split-window-vertically-instead ()
  (interactive)
  (let* ((next-buffer (window-buffer (next-window))))
    (delete-other-windows)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer next-buffer)
    (other-window 1)))

(defun split-window-horizontally-instead ()
  (interactive)
  (let* ((next-buffer (window-buffer (next-window))))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (switch-to-buffer next-buffer)
    (other-window 1)))

(defancyhydra jp-window (:hint nil :foreign-keys warn)
  (;; general window management commands
   "Windows" (("x" ace-delete-window "delete")
              ("m" ace-delete-other-windows "maximize")
              ("s" ace-swap-window "swap")
              ("a" ace-select-window "select")
              ("o" other-window "cycle")
              ("q" nil "quit")) ;;TODO fancy hydra bug: this head doesn't appear in the docstring
   ;; resize
   "Resize" (("h" move-border-left "←")
             ("j" move-border-down "↓")
             ("k" move-border-up "↑")
             ("l" move-border-right "→"))
   ;; split
   "Split"  (("b" split-window-right "horizontally")
             ("B" split-window-horizontally-instead "horizontally instead")
             ("v" split-window-below "vertically")
             ("V" split-window-vertically-instead "vertically instead"))
   ;; zoom
   "Zoom" (("+" zoom-in "in")
           ("=" zoom-in)
           ("-" zoom-out "out")
           ("0" jp-zoom-default "reset"))))

(provide 'jp-window)
;;; jp-window.el ends here
