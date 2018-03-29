;;; jp-clojure.el --- Clojure configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; major mode hydra
(require 'major-mode-hydra)

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
  :init
  (progn
    (major-mode-hydra-bind clojure-mode "Connect"
      ("j" cider-jack-in "jack-in")
      ("J" cider-jack-in-clojurescript "jack-in-cljs")
      ("c" cider-connect "connect")
      ("R" cider-restart "restart")
      ("Q" cider-quit "quit"))
    (major-mode-hydra-bind clojure-mode "Load"
      ("k" cider-load-buffer "buffer")
      ("l" cider-load-file "file")
      ("L" cider-load-all-project-ns "all-ns")
      ("r" cider-refresh "reload"))
    (major-mode-hydra-bind clojure-mode "Eval"
      ("e" cider-eval-last-sexp-to-repl "eval-last")
      ("f" cider-eval-defun-at-point "eval-defun")
      ("d" (cider-eval-defun-at-point t) "debug-defun")
      ("i" cider-interrupt "interrupt"))
    (major-mode-hydra-bind clojure-mode "Test"
      ("t" cider-test-run-ns-tests "ns")
      ("T" cider-test-run-loaded-tests "loaded")
      ("F" cider-test-rerun-failed-tests "failed"))
    (major-mode-hydra-bind clojure-mode "Find"
      ("n" cider-find-ns "ns"))
    (major-mode-hydra-bind clojure-mode "Docs"
      ("d" cider-doc "doc")))

  :commands (cider-jack-in
             cider-jack-in-clojurescript
             cider-connect)

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
    (setq cider-show-error-buffer nil)))

(use-package cider-apropos
  :init
  (progn
    (major-mode-hydra-bind clojure-mode "Docs"
      ("a" cider-apropos "apropos")))
  :commands (cider-apropos))

(use-package cider-macroexpansion
  :init
  (progn
    (major-mode-hydra-bind clojure-mode "Macros"
      ("x" cider-macroexpand-1 "expand-1")
      ("X" cider-macroexpand-all "expand-all")))
  :commands (cider-macroexpand-1 cider-macroexpand-all))

(provide 'jp-clojure)
;;; jp-clojure.el ends here
