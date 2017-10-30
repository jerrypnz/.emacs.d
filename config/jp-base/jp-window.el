(require 'hydra)
(require 'ace-window)

(defun jp-ace-split-window-vert ()
  (interactive)
  (aw-select " Ace - Split Windows Vertically"
             #'aw-split-window-vert))

(defun jp-ace-split-window-horz ()
  (interactive)
  (aw-select " Ace - Split Windows Horizontally"
             #'aw-split-window-horz))

(defun jp-ace-split-window-fair ()
  (interactive)
  (aw-select " Ace - Split Windows Fairly"
             #'aw-split-window-fair))

(defun jp-text-scale-reset ()
  (interactive)
  (text-scale-increase 0))

(defhydra jp-window (:hint nil :foreign-keys warn)
  "
 Windows^^        Resize^^   Split        ^^    Zoom
──────^^──────── ───────^^─ ──────────────^^── ──────^^───────
 [_x_] delete     [_h_] ←    [_z_] horizontal   [_+_] in
 [_m_] maximize   [_j_] ↓    [_v_] vertical     [_-_] out
 [_s_] swap       [_k_] ↑    [_f_] fair         [_0_] default
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
  ("h" shrink-window-horizontally)
  ("j" shrink-window)
  ("k" enlarge-window)
  ("l" enlarge-window-horizontally)
  ;; split
  ("z" jp-ace-split-window-horz)
  ("v" jp-ace-split-window-vert)
  ("f" jp-ace-split-window-fair)
  ;; zoom
  ("+" text-scale-increase)
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("0" jp-text-scale-reset)
  ("q" nil))

(provide 'jp-window)
