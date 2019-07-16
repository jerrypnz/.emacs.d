;;; jp-go.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package go-mode
  :straight t
  :mode "\\.go\\'")

(use-package jp-go-tests
  :after (go-mode)
  :mode-hydra
  (go-mode nil
           ("Test"
            (("tt" jp-go-run-test-current-function "current function")
             ("ts" jp-go-run-test-current-suite "current suite")
             ("tp" jp-go-run-package-tests "package")
             ("tP" jp-go-run-package-tests-nested "package nested")))))

(use-package jp-go-play
  :commands (go-play))

(provide 'jp-go)
;;; jp-go.el ends here
