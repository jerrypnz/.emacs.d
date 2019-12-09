;;; jp-elfeed.el --- elfeed configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package elfeed
  :straight t
  :commands (elfeed))

(use-package elfeed-org
  :straight t
  :after (elfeed)
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list (concat user-emacs-directory "feeds.org"))))

(provide 'jp-elfeed)
;;; jp-elfeed.el ends here
