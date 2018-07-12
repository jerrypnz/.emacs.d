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
  :straight t
  :mode "\\.go\\'"
  :bind (:map go-mode-map
              ("M-." . godef-jump))
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'go-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'gofmt-before-save)))

    (major-mode-hydra-bind go-mode "Doc"
      ("d" godoc-at-point "doc at point"))
    (major-mode-hydra-bind go-mode "Imports"
      ("ia" go-import-add "add")
      ("ir" go-remove-unused-imports "cleanup"))))

(use-package company-go
  :if (jp-init-gocode-emacs-path)
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode))))

(use-package go-eldoc
  :straight t
  :after (go-mode)
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-rename
  :straight t
  :after (go-mode)
  :commands (go-rename)
  :init
  (major-mode-hydra-bind go-mode "Refactor"
    ("r" go-rename "rename")))

(use-package jp-go-tests
  :after (go-mode)
  :config
  (major-mode-hydra-bind go-mode "Test"
    ("tt" jp-go-run-test-current-function "current function")
    ("ts" jp-go-run-test-current-suite "current suite")
    ("tp" jp-go-run-package-tests "package")
    ("tP" jp-go-run-package-tests-nested "package nested")))

(use-package go-guru
  :straight t
  :after (go-mode)
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
        (setq go-guru-scope relative-package-path)))

    (major-mode-hydra-bind go-mode "Guru"
      ("D" go-guru-describe "describe")
      ("R" go-guru-referrers "referrers")
      ("I" go-guru-implements "implements")
      ("F" go-guru-freevars "freevars")
      ("C" go-guru-callers "callers")
      ("E" go-guru-callees "callees")
      ("T" go-guru-callstack "callstack")
      ("P" go-guru-pointsto "pointsto")
      ("W" go-guru-whicherrs "whicherrs")
      ("H" go-guru-peers "peers")
      ("S" jp-go-guru-set-current-package-as-main "set scope"))))

(use-package jp-go-play
  :commands (go-play))

(provide 'jp-go)
;;; jp-go.el ends here
