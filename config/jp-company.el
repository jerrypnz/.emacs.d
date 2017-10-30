;;; jp-company.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; company
(use-package company
  :config
  (progn
    (define-key company-active-map (kbd "RET") nil)
    (setq company-idle-delay 0.125
          company-minimum-prefix-length 1
          company-require-match nil
          company-transformers '(company-sort-by-occurrence)
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                              company-preview-frontend
                              company-echo-metadata-frontend))
    (global-company-mode t)))

(provide 'jp-company)
;;; jp-company.el ends here
