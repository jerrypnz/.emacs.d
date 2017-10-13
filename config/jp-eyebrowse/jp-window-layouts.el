(require 'hydra)
(require 'eyebrowse)

(defhydra jp-window-layouts (:hint nil)
  "
 Layouts^^^^      Actions       ^^^^
 ───────^^^^───── ──────────────^^^^────────────────
 [_1_] [_5_]      [_n_] next    [_p_] previous
 [_2_] [_6_]      [_o_] other   [_x_] close
 [_3_] [_7_]      [_r_] rename
 [_4_] [_8_]      [_q_] quit

current: %s(eyebrowse--get 'current-slot)
"
  ("1" eyebrowse-switch-to-window-config-1 :exit t)
  ("2" eyebrowse-switch-to-window-config-2 :exit t)
  ("3" eyebrowse-switch-to-window-config-3 :exit t)
  ("4" eyebrowse-switch-to-window-config-4 :exit t)
  ("5" eyebrowse-switch-to-window-config-5 :exit t)
  ("6" eyebrowse-switch-to-window-config-6 :exit t)
  ("7" eyebrowse-switch-to-window-config-7 :exit t)
  ("8" eyebrowse-switch-to-window-config-8 :exit t)
  ("n" eyebrowse-next-window-config)
  ("p" eyebrowse-prev-window-config)
  ("o" eyebrowse-last-window-config)
  ("r" eyebrowse-rename-window-config)
  ("x" eyebrowse-close-window-config)
  ("q" nil))

(provide 'jp-window-layouts)
