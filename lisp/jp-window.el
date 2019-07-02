;;; jp-window.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'pretty-hydra)
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

(defvar jp-window--title)
(setq jp-window--title (with-faicon "windows" "Window Management" 1 -0.05))

(pretty-hydra-define jp-window (:hint nil :foreign-keys warn :title jp-window--title)
  (;; general window management commands
   "Actions"
   (("TAB" other-window "switch")
    ("x" ace-delete-window "delete")
    ("m" ace-delete-other-windows "maximize")
    ("s" ace-swap-window "swap")
    ("a" ace-select-window "select")
    ("q" nil "quit"))

   ;; resize
   "Resize"
   (("h" move-border-left "←")
    ("j" move-border-down "↓")
    ("k" move-border-up "↑")
    ("l" move-border-right "→")
    ("n" balance-windows "balance")
    ("f" toggle-frame-fullscreen "toggle fullscreen"))

   ;; split
   "Split"
   (("b" split-window-right "horizontally")
    ("B" split-window-horizontally-instead "horizontally instead")
    ("v" split-window-below "vertically")
    ("V" split-window-vertically-instead "vertically instead"))

   ;; zoom
   "Zoom"
   (("+" zoom-in "in")
    ("=" zoom-in)
    ("-" zoom-out "out")
    ("0" jp-zoom-default "reset"))))

(provide 'jp-window)
;;; jp-window.el ends here
