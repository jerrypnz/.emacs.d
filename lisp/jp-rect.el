;;; jp-rect.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'pretty-hydra)

(defvar jp-rectangle--title)
(setq jp-rectangle--title (with-material "grid_on" "Rectangle" 1 -0.1))

(defvar-local jp-rectangle--highlight-disabled nil)

;; Taken from https://github.com/abo-abo/hydra/wiki/Rectangle-Operations
(pretty-hydra-define jp-rectangle (:body-pre (progn
                                               (when (bound-and-true-p symbol-overlay-mode)
                                                 (symbol-overlay-mode -1)
                                                 (setq-local jp-rectangle--highlight-disabled t))
                                               (rectangle-mark-mode 1))
                                   :title jp-rectangle--title
                                   :color pink
                                   :post (progn
                                           (deactivate-mark)
                                           (when jp-rectangle--highlight-disabled
                                             (symbol-overlay-mode +1)))
                                   :foreign-keys warn)
  ("Select"
   (("k" rectangle-previous-line "↑")
    ("j" rectangle-next-line "↓")
    ("h" rectangle-backward-char "←")
    ("l" rectangle-forward-char "→")
    ("e" rectangle-exchange-point-and-mark "exchange")
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1)) "reset"))

   "Edit"
   (("d" kill-rectangle "kill")
    ("o" open-rectangle "open")
    ("t" string-rectangle "string")
    ("c" clear-rectangle "clear")
    ("N" rectangle-number-lines "line number"))

   "Copy/Yank"
   (("w" copy-rectangle-as-kill "copy")
    ("y" yank-rectangle "yank"))

   "Undo"
   (("u" undo "undo")
    ("q" nil "quit"))))

(provide 'jp-rect)
;;; jp-rect.el ends here
