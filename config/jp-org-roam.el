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

  :config
  (org-roam-db-autosync-mode)

  :pretty-hydra
  ((:color teal :quit-key "q" :title jp-org-roam-hydra--title)
   ("Navigation"
    (("l" org-roam "roam")
     ("c" org-roam-capture "capture")
     ("f" org-roam-node-find "find"))
    "Edit"
    (("i" org-roam-node-insert "insert")))))

(provide 'jp-org-roam)
