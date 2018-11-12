;;; jp-clojure.el --- Clojure configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'major-mode-hydra)

(use-package clojure-mode
  :straight t
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode))
  :config
  (progn
    (major-mode-hydra-bind clojure-mode "Edit"
      (">" clojure-thread-first-all "thread-first")
      ("<" clojure-thread-last-all "thread-last")
      ("u" clojure-unwind-all "thread-unwind"))
    ;; If put in `define-clojure-indent', it will fail with a "wrong
    ;; type argument: listp, 1" error the first time a clj buffer is
    ;; opened.
    (put-clojure-indent '-> 1)
    (put-clojure-indent '->> 1)

    (define-clojure-indent
      ;; core.match
      (match 1)
      ;; expectations
      (expect 0)
      (expect-let 1)
      ;; clj-time
      (do-at 1)
      ;; custom core.async macros
      (go? 0)
      (go?>x 1)
      (go?> 1)
      (go-loop? 1)
      (thread? 0)
      (thread?> 1)
      (thread?>x 1)
      (some-> 1)
      (some->> 1)
      (->application 1)
      (->server 1)
      ;; metrics-clojure
      (time! 1)
      (with-logging-context 1))))

(use-package cider
  :straight t
  :init
  (progn
    (major-mode-hydra-bind clojure-mode "Connect"
      ("j" cider-jack-in "jack-in")
      ("J" cider-jack-in-clojurescript "jack-in-cljs")
      ("c" cider-connect "connect")
      ("R" cider-restart "restart")
      ("Q" cider-quit "disconnect")
      ("q" nil "quit"))
    (major-mode-hydra-bind clojure-mode "Load"
      ("k" cider-load-buffer "buffer")
      ("l" cider-load-file "file")
      ("L" cider-load-all-project-ns "all-ns")
      ("r" cider-ns-refresh "reload"))
    (major-mode-hydra-bind clojure-mode "Eval"
      ("s" cider-repl-set-ns "set-repl-ns")
      ("e" cider-eval-last-sexp-to-repl "eval-last")
      ("f" cider-eval-defun-at-point "eval-defun")
      ("I" cider-inspect-last-result "inspect-last-result")
      ("D" (cider-eval-defun-at-point t) "debug-defun")
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
    ;; smartparens mode for the REPL
    (add-hook 'cider-repl-mode-hook #'smartparens-mode)
    ;; error buffer not popping up
    (setq cider-show-error-buffer nil)

    (major-mode-hydra-bind cider-repl-mode "Connect"
      ("R" cider-restart "restart")
      ("Q" cider-quit "disconnect")
      ("q" nil "quit"))
    (major-mode-hydra-bind cider-repl-mode "Load"
      ("l" cider-load-file "file")
      ("L" cider-load-all-project-ns "all-ns")
      ("r" cider-ns-refresh "reload"))
    (major-mode-hydra-bind cider-repl-mode "REPL"
      ("s" cider-repl-set-ns "set-repl-ns")
      ("i" cider-interrupt "interrupt")
      ("c" cider-repl-clear-buffer "clear"))
    (major-mode-hydra-bind cider-repl-mode "Docs"
      ("d" cider-doc "doc"))))

(use-package jp-counsel-cider
  :after (cider)
  :init
  (progn
    (major-mode-hydra-bind clojure-mode "Docs"
      ("a" jp-counsel-cider-apropos "apropos"))
    (major-mode-hydra-bind cider-repl-mode "Docs"
      ("a" jp-counsel-cider-apropos "apropos"))
    (major-mode-hydra-bind cider-repl-mode "REPL"
      ("h" jp-counsel-cider-repl-history "search-history")
      ("H" cider-repl-history "show-history")))
  :commands (jp-counsel-cider-apropos
             jp-counsel-cider-repl-history))

(use-package cider-macroexpansion
  :after (cider)
  :init
  (progn
    (major-mode-hydra-bind clojure-mode "Macros"
      ("x" cider-macroexpand-1 "expand-1")
      ("X" cider-macroexpand-all "expand-all"))
    (major-mode-hydra-bind cider-repl-mode "Macros"
      ("x" cider-macroexpand-1 "expand-1")
      ("X" cider-macroexpand-all "expand-all")))
  :commands (cider-macroexpand-1 cider-macroexpand-all))

(use-package flycheck-clojure
  :straight t
  :after (cider flycheck)
  :config
  (flycheck-clojure-setup))

(provide 'jp-clojure)
;;; jp-clojure.el ends here
