;;; jp-rect.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'hydra)

;; Taken from https://github.com/abo-abo/hydra/wiki/Rectangle-Operations
(defhydra jp-rectangle (:body-pre (rectangle-mark-mode 1)
                                  :color pink
                                  :hint nil
                                  :post (deactivate-mark)
                                  :foreign-keys warn)
  "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines
_h_   _l_     _y_ank        _t_ype       _e_xchange-point
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark
^^^^          _u_ndo        _q_uit     ^ ^
"
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("q" nil))

(provide 'jp-rect)
;;; jp-rect.el ends here
