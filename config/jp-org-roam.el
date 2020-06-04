;;; jp-org-roam.el --- Configuration for org-roam -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package org-roam
  :straight t
  :preface
  (progn
    (defvar jp-org-roam-hydra--title)
    (setq jp-org-roam-hydra--title  (with-material "event_note" "Org Roam")))

  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-completion-system 'ivy)

  :pretty-hydra
  ((:color teal :quit-key "q" :title jp-org-roam-hydra--title)
   ("Navigation"
    (("l" org-roam "roam")
     ("f" org-roam-find-file "find file")
     ("j" org-roam-jump-to-index "jump to index")
     ("b" org-roam-switch-to-buffer "switch to buffer")
     ("g" org-roam-graph "show graph"))
    "Edit"
    (("i" org-roam-insert "insert")))))

(provide 'jp-org-roam)
