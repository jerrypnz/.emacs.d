;;; jp-clojure.el --- Clojure configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package clojure-mode
  :mode (("\\.clj\\"  . clojure-mode)
         ("\\.cljs\\" . clojurescript-mode)
         ("\\.cljc\\" . clojurec-mode)
         ("\\.cljx\\" . clojurex-mode)))

(use-package cider
  :config
  (progn
    ;; REPL history file
    (setq cider-repl-history-file "~/.emacs.d/cider-history")
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

(provide 'jp-clojure)
;;; jp-clojure.el ends here
