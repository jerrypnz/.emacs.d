;;; jp-window.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'hydra)
(require 'ace-window)
(require 'move-border)
(require 'zoom-frm)

(defun jp-zoom-default ()
  "Reset text scale."
  (interactive)
  (zoom-in/out 0))

(defhydra jp-window (:hint nil :foreign-keys warn)
  "
 Windows^^        Resize^^   Split^^            Zoom
────────────^^ ───────^^ ──────────────^^ ─────────────^^
 [_x_] delete     [_h_] ←    [_b_] horizontal   [_+_] in
 [_m_] maximize   [_j_] ↓    [_v_] vertical     [_-_] out
 [_s_] swap       [_k_] ↑    ^^                 [_0_] default
 [_a_] select     [_l_] →
 [_o_] cycle

 [_q_] quit
"
  ("x" ace-delete-window)
  ("m" ace-delete-other-windows)
  ("s" ace-swap-window)
  ("a" ace-select-window)
  ("o" other-window)
  ;; resize
  ("h" move-border-left)
  ("j" move-border-down)
  ("k" move-border-up)
  ("l" move-border-right)
  ;; split
  ("b" split-window-right)
  ("v" split-window-below)
  ;; zoom
  ("+" zoom-in)
  ("=" zoom-in)
  ("-" zoom-out)
  ("0" jp-zoom-default)
  ("q" nil))

(provide 'jp-window)
;;; jp-window.el ends here
