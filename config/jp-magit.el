;;; jp-magit.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; magit
(use-package magit
  :straight t
  :defer 2
  :init
  ;; Since we're using git subtree without building it with Make, we
  ;; don't have "magit-version.el" file.  Get around it by setting the
  ;; version here explicitly.
  (setq magit-version "2.11.0")

  :config
  (setq magit-repository-directories
        '(("~/dev/workspace/"   . 3)
          ("~/dev/personal/"    . 2)
          ("~/dev/open-source/" . 2)
          ("~/go/src/"          . 4))))

;; git-timemachine
(use-package git-timemachine
  :straight t
  :commands (git-timemachine))

(provide 'jp-magit)
;;; jp-magit.el ends here
