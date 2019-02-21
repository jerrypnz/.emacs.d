;;; jp-org-agenda.el --- org-agenda hydra etc -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'pretty-hydra)
(require 'dash)
(require 'org-agenda)

(defvar jp-org-agenda--current-view-idx nil)

(defun jp-org-agenda-open-nth-view (n)
  (-if-let (c (nth n org-agenda-custom-commands))
      (progn
        (org-agenda nil (car c))
        (setq jp-org-agenda--current-view-idx n))
    (message "Non-existent agenda view")))

(defun jp-org-agenda-nth-name (n)
  (-if-let (c (nth n org-agenda-custom-commands))
      (propertize (cadr c) 'face (if (= n jp-org-agenda--current-view-idx) 'underline 'shadow))
    ""))

(defun jp-org-agenda-maybe-open ()
  (interactive)
  (when (not (eq major-mode 'org-agenda-mode))
    (jp-org-agenda-open-nth-view 0)))

(defvar jp-org-agenda--title)
(setq jp-org-agenda--title
  (s-concat (all-the-icons-faicon "calendar" :v-adjust 0.01 :height 1.1)
            (propertize " Org Agenda\n" 'face '(:height 1.1 :weight bold))))

(pretty-hydra-define jp-org-agenda
  (:hint nil :foreign-keys run :title jp-org-agenda--title :body-pre (jp-org-agenda-maybe-open) :quit-key "q")
  ("Actions"
   (("SPC" (org-agenda-show-and-scroll-up t) "preview")
    ("TAB" org-agenda-goto "goto" :exit t)
    ("C-r" org-agenda-refile "refile")
    ("C-s" org-agenda-schedule "schedule")
    ("C-t" org-agenda-todo "todo" :exit t)
    ("C-a" org-agenda-archive "archive")
    ("n" org-agenda-next-item "next item")
    ("p" org-agenda-previous-item "previous item")
    ("k" org-agenda-capture "capture" :exit t)
    ("g" org-agenda-redo-all "refresh")
    ("s" org-save-all-org-buffers "save all"))
   "Filter"
   (("<" org-agenda-filter-by-category "by category")
    ("/" org-agenda-filter-by-tag "by tag")
    ("=" org-agenda-filter-by-regexp "by regexp")
    ("R" org-agenda-filter-remove-all "remove all"))
   "Calendar"
   (("." org-agenda-goto-today "today")
    ("D" org-agenda-day-view "day")
    ("W" org-agenda-week-view "week")
    ("M" org-agenda-month-view "month")
    ("N" org-agenda-later "later")
    ("P" org-agenda-earlier "earlier")
    ("G" org-agenda-reset-view "reset"))
   "Clock"
   (("I" org-agenda-clock-in "in")
    ("O" org-agenda-clock-out "out")
    ("C" org-agenda-clock-cancel "cancel")
    ("J" org-agenda-clock-goto "goto" :exit t))
   "Toggles"
   (("tl" org-agenda-log-mode "log")
    ("ta" (org-agenda-archives-mode
           (if org-agenda-archives-mode nil t)) "archive")
    ("tr" org-agenda-clockreport-mode "clockreport")
    ("tf" org-agenda-follow-mode "follow")
    ("td" org-agenda-toggle-diary "diary"))
   "Agenda Views"
   (("1" (jp-org-agenda-open-nth-view 0) (jp-org-agenda-nth-name 0))
    ("2" (jp-org-agenda-open-nth-view 1) (jp-org-agenda-nth-name 1))
    ("3" (jp-org-agenda-open-nth-view 2) (jp-org-agenda-nth-name 2))
    ("A" org-agenda "open dispatcher")
    ("Q" org-agenda-quit "close agenda" :exit t))))

(provide 'jp-org-agenda)
;;; jp-org-agenda.el ends here
