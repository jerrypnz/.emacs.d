;; jp-global-hydra.el Global hydra for dispatching commands -*- lexical-binding: t; -*-

(require 'hydra)

(defhydra hydra-global-menu (:color pink
                             :hint nil)
  "
^TBD^             ^TBD^               ^p: Projects^     ^j: Jump
^^^^^^^^-----------------------------------------------------------------
_m_: tbd          _u_: unmark        _p_: execute       _c_: re-isearch
_s_: tbd          _U_: unmark up     _b_: bury          _I_: isearch
_d_: tbd          ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up    ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(defalias 'jp-global-menu 'hydra-global-menu/body)


