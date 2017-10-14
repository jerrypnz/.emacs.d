(require 'hydra)
(require 'eyebrowse)

(defun jp-eyebrowse-layout-tag (slot)
  (let* ((window-configs (eyebrowse--get 'window-configs))
         (window-config (assoc slot window-configs))
         (tag (nth 2 window-config))
         (name (cond
                ((not tag)          "") ; show nothing if it's not created yet
                ((= (length tag) 0) "workspace") ; default name if there is no tag
                (t                  tag)))
         (face (if (= slot (eyebrowse--get 'current-slot))
                   '(underline t)
                 'shadow)))
    (propertize name 'face face)))

(defhydra jp-window-layouts (:hint nil)
  "
 Actions     ^^    Layouts^^
 ────────────^^─── ───────^^─────
 [_n_] next        [_1_] ?1?
 [_p_] previous    [_2_] ?2?
 [_o_] other       [_3_] ?3?
 [_r_] rename      [_4_] ?4?
 [_x_] close       [_5_] ?5?
 [_q_] quit        [_6_] ?6?
"
  ("1" eyebrowse-switch-to-window-config-1 (jp-eyebrowse-layout-tag 1) :exit t)
  ("2" eyebrowse-switch-to-window-config-2 (jp-eyebrowse-layout-tag 2) :exit t)
  ("3" eyebrowse-switch-to-window-config-3 (jp-eyebrowse-layout-tag 3) :exit t)
  ("4" eyebrowse-switch-to-window-config-4 (jp-eyebrowse-layout-tag 4) :exit t)
  ("5" eyebrowse-switch-to-window-config-5 (jp-eyebrowse-layout-tag 5) :exit t)
  ("6" eyebrowse-switch-to-window-config-6 (jp-eyebrowse-layout-tag 6) :exit t)
  ("n" eyebrowse-next-window-config)
  ("p" eyebrowse-prev-window-config)
  ("o" eyebrowse-last-window-config)
  ("r" eyebrowse-rename-window-config)
  ("x" eyebrowse-close-window-config)
  ("q" nil))

(provide 'jp-window-layouts)
