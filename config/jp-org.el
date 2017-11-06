;;; jp-org.el --- Configuration for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package org
  :defer t
  :bind
  (("M-m c" . org-capture)
   ("M-m a" . org-agenda))

  :config
  (progn
    (defconst jp-work-org-file "~/org/work.org")
    (defconst jp-life-org-file "~/org/life.org")
    (defconst jp-inbox-org-file "~/org/inbox.org")
    (defconst jp-diary-org-fle "~/org/diary.org")

    (setq org-directory "~/org")
    (setq org-default-notes-file jp-inbox-org-file)

    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))

    (setq org-todo-state-tags-triggers
          '(("CANCELLED" ("CANCELLED" . t))
            ("WAITING" ("WAITING" . t))
            ("HOLD" ("WAITING") ("HOLD" . t))
            (done ("WAITING") ("HOLD"))
            ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
            ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
            ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

    (setq org-capture-templates
          `(("t" "Todo" entry (file org-default-notes-file)
             "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
            ("m" "Meeting" entry (file org-default-notes-file)
             "* Meeting notes for %? :MEETING:\n%t" :clock-in t :clock-resume t)
            ("d" "Diary" entry (file+datetree ,jp-diary-org-fle)
             "* %?\n%U\n" :clock-in t :clock-resume t)
            ("i" "Idea" entry (file org-default-notes-file)
             "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
            ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
             "** NEXT %? \nDEADLINE: %t")))

    (setq org-agenda-files (list jp-work-org-file
                                 jp-life-org-file
                                 jp-inbox-org-file))

    (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                     (org-agenda-files :maxlevel . 9))))

    (setq org-startup-folded nil)))


(provide 'jp-org)
;;; jp-org.el ends here
