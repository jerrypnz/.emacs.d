;;; jp-org.el --- Configuration for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(defconst jp-work-org-file "~/org/work.org")
(defconst jp-private-org-file "~/org/private.org")
(defconst jp-inbox-org-file "~/org/inbox.org")
(defconst jp-diary-org-fle "~/org/diary.org")
(defconst jp-notes-dir "~/org/notes")


(use-package org
  :defer t
  :bind
  (("M-m c" . org-capture)
   ("M-m a" . org-agenda)
   :map org-mode-map
   ("RET" . org-return-indent))

  :config
  (progn
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

    (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

    (setq org-capture-templates
          `(("t" "TODO" entry (file org-default-notes-file)
             "* TODO %?\n%u\n" :clock-in t :clock-resume t)
            ("m" "Meeting" entry (file org-default-notes-file)
             "* Meeting notes for %? :MEETING:\n%t" :clock-in t :clock-resume t)
            ("d" "Diary" entry (file+datetree ,jp-diary-org-fle)
             "* %?\n%U\n" :clock-in t :clock-resume t)
            ("i" "Idea" entry (file org-default-notes-file)
             "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
            ("n" "Next Task" entry (file org-default-notes-file)
             "** NEXT %? \nDEADLINE: %t")))

    (setq org-agenda-custom-commands
          '(("a" "Agenda"
             ((agenda "-REFILE")
              (alltodo "-REFILE")
              (tags "REFILE"
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-overriding-header "Inbox:")))))))

    (setq org-agenda-files (list jp-work-org-file
                                 jp-private-org-file
                                 jp-inbox-org-file))

    (setq org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9)))

    (setq org-startup-folded nil)))


(use-package deft
  :bind ("M-m n" . deft)
  :commands (deft)
  :config
  (progn
    (setq deft-extensions '("org"))
    (setq deft-default-extension "org")
    (setq deft-directory jp-notes-dir)
    (setq deft-org-mode-title-prefix t)
    (setq deft-use-filter-string-for-filename t)
    (setq deft-use-filename-as-title nil)
    (setq deft-file-naming-rules
          '((noslash . "-")
            (nospace . "-")
            (case-fn . downcase)))))


(provide 'jp-org)
;;; jp-org.el ends here
