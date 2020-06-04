;;; jp-org-roam.el --- Configuration for org-journal -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package org-journal
  :straight t
  :preface
  (progn
    (defvar jp-org-journal-hydra--title)
    (setq jp-org-journal-hydra--title  (with-faicon "calendar" "Org Journal")))
  :pretty-hydra
  ((:color teal :quit-key "q" :title jp-org-journal-hydra--title)
   ("Journal Entries"
    (("i" org-journal-new-entry "new entry")
     ("n" org-journal-open-next-entry "next entry")
     ("p" org-journal-open-previous-entry "previous entry")
     ("s" org-journal-search "search"))))
  :custom
  (org-journal-file-type 'weekly)
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/org/roam/journal/")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-file-header "#+TITLE: Weekly Journal %Y W%W\n#+STARTUP: folded\n"))

(provide 'jp-org-journal)
