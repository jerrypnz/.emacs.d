;;; jp-org-agenda.el --- org-agenda hydra etc -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'pretty-hydra)
(require 'dash)

(autoload 'org-agenda "org-agenda")
(autoload 'org-agenda-quit "org-agenda")
(defvar org-agenda-custom-commands)

(defvar jp-org-agenda--current-view-idx nil)

(defun jp-org-agenda-open-nth-view (n)
  (-if-let (c (nth n org-agenda-custom-commands))
      (progn
        (org-agenda nil (car c))
        (setq jp-org-agenda--current-view-idx n))
    (setq jp-org-agenda--current-view-idx nil)))

(defun jp-org-agenda-nth-name (n)
  (-if-let (c (nth n org-agenda-custom-commands))
      (propertize (cadr c) 'face (if (= n jp-org-agenda--current-view-idx) 'underline 'shadow))
    ""))

(pretty-hydra-define jp-org-agenda
  (:hint nil :foreign-keys run :quit-key "q"
         :pre (jp-org-agenda-open-nth-view 0)
         :post (org-agenda-quit))
  ("Agenda Views"
   (("1" (jp-org-agenda-open-nth-view 0) (jp-org-agenda-nth-name 0))
    ("2" (jp-org-agenda-open-nth-view 1) (jp-org-agenda-nth-name 1))
    ("3" (jp-org-agenda-open-nth-view 2) (jp-org-agenda-nth-name 2)))))

(provide 'jp-org-agenda)
;;; jp-org-agenda.el ends here
