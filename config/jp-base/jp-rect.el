;;; jp-rect.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'jp-fancy-hydra)

;; Taken from https://github.com/abo-abo/hydra/wiki/Rectangle-Operations
(defancyhydra jp-rectangle (:body-pre (rectangle-mark-mode 1)
                                      :color pink
                                      :hint nil
                                      :post (deactivate-mark)
                                      :foreign-keys warn)
  ("Select" (("k" rectangle-previous-line "↑")
             ("j" rectangle-next-line "↓")
             ("h" rectangle-backward-char "←")
             ("l" rectangle-forward-char "→")
             ("e" rectangle-exchange-point-and-mark "exchange point")
             ("r" (if (region-active-p)
                      (deactivate-mark)
                    (rectangle-mark-mode 1)) "reset selection"))
   "Edit" (("d" kill-rectangle "kill")                    ;; C-x r k
           ("y" yank-rectangle "yank")                    ;; C-x r y
           ("w" copy-rectangle-as-kill "copy")            ;; C-x r M-w
           ("o" open-rectangle "open")                    ;; C-x r o
           ("t" string-rectangle "string")                  ;; C-x r t
           ("c" clear-rectangle "clear")                   ;; C-x r c
           ("N" rectangle-number-lines "line number")            ;; C-x r N
           )

   "Undo" (("u" undo "undo")
           ("q" nil "quit"))))

(provide 'jp-rect)
;;; jp-rect.el ends here
