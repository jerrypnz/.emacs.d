;;; jp-go.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; gocode emacs-company package is shipped with gocode itself. try to find it in $GOPATH
;; and add it to the load path.
(defun jp-init-gocode-emacs-path ()
  (let* ((go-path (getenv "GOPATH"))
         (gocode-company-path (concat go-path "/src/github.com/nsf/gocode/emacs-company")))
    (if (file-directory-p gocode-company-path)
        (add-to-list 'load-path gocode-company-path)
      (progn
        (message "WARN: company-go not found in $GOPATH")
        nil))))

(use-package go-mode
  :mode "\\.go\\'"
  :bind (:map go-mode-map
              ("M-." . godef-jump))
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'go-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'gofmt-before-save)))))

(use-package company-go
  :if (jp-init-gocode-emacs-path)
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode))))

(use-package go-rename
  :bind (:map go-mode-map
              ;;TODO Replace it with major mode hydra
              ("C-M-m r" . go-rename)))

(use-package jp-go-play
  :commands (go-play))

(provide 'jp-go)
;;; jp-go.el ends here
