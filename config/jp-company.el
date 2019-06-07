;;; jp-company.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; company
(use-package company
  :straight t
  :config
  (progn
    (define-key company-active-map (kbd "RET") nil)
    (setq company-idle-delay 0.125
          company-minimum-prefix-length 1
          company-require-match nil
          company-transformers '(company-sort-by-occurrence)
          company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                              company-preview-frontend
                              company-echo-metadata-frontend))
    (global-company-mode t)))

(use-package company-box
  :straight t
  :defer t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-enable-icon nil))

(use-package company-dabbrev
  :after (company)
  :config
  (progn
    (setq company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil)))

(use-package company-yasnippet
  :after (company)
  :bind
  ("C-M-y" . company-yasnippet))

(provide 'jp-company)
;;; jp-company.el ends here
