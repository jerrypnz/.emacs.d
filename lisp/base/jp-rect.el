;;; jp-rect.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'pretty-hydra)

;; Taken from https://github.com/abo-abo/hydra/wiki/Rectangle-Operations
(pretty-hydra-define jp-rectangle (:body-pre (rectangle-mark-mode 1)
                                             :color pink
                                             :hint nil
                                             :post (deactivate-mark)
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
