;;; jp-help.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'pretty-hydra)
(require 'helpful)

(pretty-hydra-define jp-help
  (:color teal :quit-key "q" :hint nil)
  ("Help"
   (("f" helpful-callable "callable")
    ("v" helpful-variable "variable")
    ("k" helpful-key "key")
    ("c" helpful-command "command")
    ("d" helpful-at-point "thing at point"))))

(provide 'jp-help)
;;; jp-help.el ends here
