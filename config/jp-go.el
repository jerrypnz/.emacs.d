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

(use-package go-guru
  :bind (:map go-mode-map
              ;;TODO Replace it with major mode hydra
              ("C-M-m R" . go-guru-referrers)
              ("C-M-m I" . go-guru-implements)
              ("C-M-m F" . go-guru-freevars)
              ("C-M-m D" . go-guru-describe)
              ("C-M-m C" . go-guru-callers)
              ("C-M-m E" . go-guru-callees)
              ("C-M-m T" . go-guru-callstack)
              ("C-M-m P" . go-guru-pointsto)
              ("C-M-m W" . go-guru-whicherrs)
              ("C-M-m H" . go-guru-peers)
              ("C-M-m S" . jp-go-guru-set-current-package-as-main))
  :config
  (progn
    ;; Taken from https://gist.github.com/sideshowcoder/0d37c53bbf1d62299600bb723cc20af0
    (defun jp-go-guru-set-current-package-as-main ()
      "GoGuru requires the scope to be set to a go package which
   contains a main, this function will make the current package the
   active go guru scope, assuming it contains a main"
      (interactive)
      (let* ((filename (buffer-file-name))
             (gopath-src-path (concat (file-name-as-directory (go-guess-gopath)) "src"))
             (relative-package-path (directory-file-name (file-name-directory (file-relative-name filename gopath-src-path)))))
        (setq go-guru-scope relative-package-path)))))

(use-package jp-go-play
  :commands (go-play))

(provide 'jp-go)
;;; jp-go.el ends here
