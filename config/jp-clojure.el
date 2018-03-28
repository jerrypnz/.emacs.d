;;; jp-clojure.el --- Clojure configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package clojure-mode
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljx\\'" . clojurex-mode))
  :config
  (progn
    (define-clojure-indent
      ;; expectations
      (expect 0))))

(use-package cider
  :config
  (progn
    ;; REPL history file
    (setq cider-repl-history-file "~/.emacs.d/cider-history")
    ;; Don't prompt for symbol by default
    (setq cider-prompt-for-symbol nil)
    ;; nice pretty printing
    (setq cider-repl-use-pretty-printing t)
    ;; nicer font lock in REPL
    (setq cider-repl-use-clojure-font-lock t)
    ;; result prefix for the REPL
    (setq cider-repl-result-prefix ";; => ")
    ;; never ending REPL history
    (setq cider-repl-wrap-history t)
    ;; looong history
    (setq cider-repl-history-size 3000)
    ;; eldoc for clojure
    (add-hook 'cider-mode-hook #'eldoc-mode)
    ;; error buffer not popping up
    (setq cider-show-error-buffer nil)

    ;; major mode hydra
    (require 'major-mode-hydra)

    (major-mode-hydra-bind-key 'clojure-mode "Connect" "j" #'cider-jack-in "jack-in")
    (major-mode-hydra-bind-key 'clojure-mode "Connect" "J" #'cider-jack-in-clojurescript "jack-in-cljs")
    (major-mode-hydra-bind-key 'clojure-mode "Connect" "c" #'cider-connect "connect")
    (major-mode-hydra-bind-key 'clojure-mode "Connect" "R" #'cider-restart "restart")
    (major-mode-hydra-bind-key 'clojure-mode "Connect" "Q" #'cider-quit "quit")

    (major-mode-hydra-bind-key 'clojure-mode "Load" "k" #'cider-load-buffer "buffer")
    (major-mode-hydra-bind-key 'clojure-mode "Load" "l" #'cider-load-file "file")
    (major-mode-hydra-bind-key 'clojure-mode "Load" "L" #'cider-load-all-project-ns "all-ns")
    (major-mode-hydra-bind-key 'clojure-mode "Load" "r" #'cider-refresh "reload")

    (major-mode-hydra-bind-key 'clojure-mode "Eval" "e" #'cider-eval-last-sexp-to-repl "eval-last")
    (major-mode-hydra-bind-key 'clojure-mode "Eval" "f" #'cider-eval-defun-at-point "eval-defun")
    (major-mode-hydra-bind-key 'clojure-mode "Eval" "d" '(cider-eval-defun-at-point t) "debug-defun")
    (major-mode-hydra-bind-key 'clojure-mode "Eval" "i" #'cider-interrupt "interrupt")

    (major-mode-hydra-bind-key 'clojure-mode "Test" "t" #'cider-test-run-ns-tests "ns")
    (major-mode-hydra-bind-key 'clojure-mode "Test" "T" #'cider-test-run-loaded-tests "loaded")
    (major-mode-hydra-bind-key 'clojure-mode "Test" "F" #'cider-test-rerun-failed-tests "failed")

    (major-mode-hydra-bind-key 'clojure-mode "Find" "n" #'cider-find-ns "ns")
    ;;(major-mode-hydra-bind-key 'clojure-mode "Find" "K" #'cider-find-keyword "keyword")

    (major-mode-hydra-bind-key 'clojure-mode "Docs" "d" #'cider-doc "doc")))

(use-package cider-apropos
  :config
  (progn
    ;; major mode hydra
    (require 'major-mode-hydra)

    (major-mode-hydra-bind-key 'clojure-mode "Docs" "a" #'cider-apropos "apropos")))

(use-package cider-macroexpansion
  :config
  (progn
    ;; major mode hydra
    (require 'major-mode-hydra)

    (major-mode-hydra-bind-key 'clojure-mode "Macros" "x" #'cider-macroexpand-1 "expand-1")
    (major-mode-hydra-bind-key 'clojure-mode "Macros" "X" #'cider-macroexpand-1 "expand-all")))

(provide 'jp-clojure)
;;; jp-clojure.el ends here
