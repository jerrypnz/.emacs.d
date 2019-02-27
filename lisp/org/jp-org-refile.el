;;; jp-org-refile.el --- Custom org-refile stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'org)

(defvar jp-org-refile-contexts nil)

(defun jp-org-refile--get-context-targets ()
  (or (cdr (-first (-lambda ((func . _))
                     (funcall func))
                   jp-org-refile-contexts))
      org-refile-targets))

(defun jp-org-refile-with-context (refile-func &rest args)
  (interactive "P")
  (let ((org-refile-targets (jp-org-refile--get-context-targets)))
    (apply refile-func args)))

(advice-add #'org-refile :around #'jp-org-refile-with-context)

(provide 'jp-org-refile)
;;; jp-org-refile.el ends here
