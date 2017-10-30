(require 'hydra)
(require 'eyebrowse)

(defun jp-eyebrowse-layout-tag (slot)
  (let* ((window-configs (eyebrowse--get 'window-configs))
         (window-config (assoc slot window-configs))
         (tag (nth 2 window-config))
         (name (cond
                ((not tag)          "") ; show nothing if it's not created yet
                ((= (length tag) 0) "default") ; default name if there is no tag
                (t                  tag)))
         (face (if (= slot (eyebrowse--get 'current-slot))
                   '(underline t)
                 'shadow)))
    (propertize name 'face face)))

(defhydra jp-layouts (:hint nil)
  "
   Actions     ^^    Layouts^^
  ───────────^^─── ───────^^─────
   [_n_] next        [_a_] ?a?
   [_p_] previous    [_s_] ?s?
   [_o_] other       [_d_] ?d?
   [_r_] rename      [_f_] ?f?
   [_x_] close       [_j_] ?j?
   [_q_] quit        [_k_] ?k?
"
  ("a" eyebrowse-switch-to-window-config-1 (jp-eyebrowse-layout-tag 1) :exit t)
  ("s" eyebrowse-switch-to-window-config-2 (jp-eyebrowse-layout-tag 2) :exit t)
  ("d" eyebrowse-switch-to-window-config-3 (jp-eyebrowse-layout-tag 3) :exit t)
  ("f" eyebrowse-switch-to-window-config-4 (jp-eyebrowse-layout-tag 4) :exit t)
  ("j" eyebrowse-switch-to-window-config-5 (jp-eyebrowse-layout-tag 5) :exit t)
  ("k" eyebrowse-switch-to-window-config-6 (jp-eyebrowse-layout-tag 6) :exit t)
  ("n" eyebrowse-next-window-config)
  ("p" eyebrowse-prev-window-config)
  ("o" eyebrowse-last-window-config)
  ("r" eyebrowse-rename-window-config)
  ("x" eyebrowse-close-window-config)
  ("q" nil))

(provide 'jp-layouts)
