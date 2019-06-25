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
  :init
  (progn
    (pretty-hydra-define jp-clojure-edit
      (:hint nil :color teal :quite-key "q" :title "Edit Clojure Source")
      ("Basic"
       (("SPC" clojure-align "align")
        ("p" clojure-cycle-privacy "cycle privacy")
        ("f" clojure-cycle-if "cycle if/if-not")
        ("w" clojure-cycle-when "cycle when/when-not")
        (":" clojure-toggle-keyword-string "toggle keyword/string")
        ("!" clojure-cycle-not "cycle not"))
       "Let"
       (("ll" (clojure-introduce-let 1) "introduce let")
        ("lm" clojure-move-to-let "move to let")
        ("lf" clojure-let-forward-slurp-sexp "forward slurp into")
        ("lb" clojure-let-backward-slurp-sexp "backward slurp into"))
       "Namespaces"
       (("nn" clojure-insert-ns-form "insert ns form")
        ("nh" clojure-insert-ns-form-at-point "insert ns form here")
        ("np" clojure-update-ns "update ns form")
        ("ns" clojure-sort-ns "sort ns"))
       "Collections"
       (("cl" clojure-convert-collection-to-list "to list")
        ("cq" clojure-convert-collection-to-quoted-list "to quoted list")
        ("cm" clojure-convert-collection-to-map "to map")
        ("cv" clojure-convert-collection-to-vector "to vector")
        ("cs" clojure-convert-collection-to-set "to set"))
       "Threading"
       (("tt" clojure-thread "thread once")
        ("tf" clojure-thread-first-all "thread all ->")
        ("tl" clojure-thread-last-all "thread all ->>")
        ("tw" clojure-unwind "unwind once")
        ("tu" clojure-unwind-all "unwind all"))))

    (major-mode-hydra-define+ (clojure-mode clojurescript-mode clojurec-mode) nil
      ("Clojure"
       ((">" clojure-thread-first-all "thread-first")
        ("<" clojure-thread-last-all "thread-last")
        ("u" clojure-unwind-all "thread-unwind")
        (":" clojure-toggle-keyword-string "toggle keyword/string")
        ("SPC" clojure-align "align")
        ("TAB" jp-clojure-edit/body "more...")))))

  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode))

  :config
  (progn

    ;; If put in `define-clojure-indent', it will fail with a "wrong
    ;; type argument: listp, 1" error the first time a clj buffer is
    ;; opened.
    (put-clojure-indent '-> 1)
    (put-clojure-indent '->> 1)

    (add-to-list 'clojure-align-cond-forms "more-of")

    (define-clojure-indent
      ;; core.match
      (match 1)
      ;; expectations
      (expect 0)
      (expect-let 1)
      (more-of 1)
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
      (with-logging-context 1)
      ;; hiccup
      (xhtml 1)
      (html5 1))))

(use-package cider
  :straight t
  :init
  (progn
    (major-mode-hydra-define+ clojure-mode nil
      ("Connect"
       (("j" cider-jack-in "jack-in")
        ("J" cider-jack-in-clojurescript "jack-in-cljs")
        ("c" cider-connect "connect")
        ("R" cider-restart "restart")
        ("Q" cider-quit "disconnect"))
       "Load"
       (("k" cider-load-buffer "buffer")
        ("l" cider-load-file "file")
        ("L" cider-load-all-project-ns "all-ns")
        ("g" cider-ns-refresh "reload"))
       "Eval"
       (("s" cider-repl-set-ns "set-repl-ns")
        ("e" cider-eval-last-sexp-to-repl "eval-last")
        ("f" cider-eval-defun-at-point "eval-defun")
        ("I" cider-inspect-last-result "inspect-last-result")
        ("D" (cider-eval-defun-at-point t) "debug-defun")
        ("i" cider-interrupt "interrupt"))
       "Test"
       (("t" cider-test-run-ns-tests "ns")
        ("T" cider-test-run-loaded-tests "loaded")
        ("F" cider-test-rerun-failed-tests "failed"))
       "Find"
       (("n" cider-find-ns "ns")
        ("d" cider-doc "doc"))))

    (major-mode-hydra-define+ cider-repl-mode nil
      ("Connect"
       (("R" cider-restart "restart")
        ("Q" cider-quit "disconnect")
        ("q" nil "quit"))
       "Load"
       (("l" cider-load-file "file")
        ("L" cider-load-all-project-ns "all-ns")
        ("g" cider-ns-refresh "reload"))
       "REPL"
       (("s" cider-repl-set-ns "set-repl-ns")
        ("i" cider-interrupt "interrupt")
        ("c" cider-repl-clear-buffer "clear"))
       "Find"
       (("d" cider-doc "doc")))))

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
    (setq cider-repl-display-help-banner nil)
    ;; eldoc for clojure
    (add-hook 'cider-mode-hook #'eldoc-mode)
    ;; smartparens mode for the REPL
    (add-hook 'cider-repl-mode-hook #'smartparens-mode)
    ;; error buffer not popping up
    (setq cider-show-error-buffer nil)))

(use-package jp-counsel-cider
  :after (cider)
  :init
  (progn
    (major-mode-hydra-define+ (clojure-mode cider-repl-mode) nil
      ("Find"
       (("a" jp-counsel-cider-apropos "apropos"))))

    (major-mode-hydra-define+ cider-repl-mode nil
      ("REPL"
       (("h" jp-counsel-cider-repl-history "search-history")
        ("H" cider-repl-history "show-history")))))

  :commands (jp-counsel-cider-apropos
             jp-counsel-cider-repl-history))

(use-package cider-macroexpansion
  :after (cider)
  :init
  (major-mode-hydra-define+ (clojure-mode cider-repl-mode) nil
    ("Eval"
     (("x" cider-macroexpand-1 "macroexpand-1")
      ("X" cider-macroexpand-all "macroexpand-all"))))

  :commands (cider-macroexpand-1 cider-macroexpand-all))

(use-package inf-clojure
  :straight t
  :init
  (major-mode-hydra-define+ clojurescript-mode nil
    ("Connect"
     (("j" (progn (inf-clojure) (inf-clojure-minor-mode +1)) "jack in")
      ("J" (progn (inf-clojure "planck -d") (inf-clojure-minor-mode +1)) "jack in (planck)")
      ("c" (progn (inf-clojure-connect) (inf-clojure-minor-mode +1)) "connect")
      ("Q" inf-clojure-quit "disconnect"))
     "Load"
     (("k" inf-clojure-eval-buffer "buffer")
      ("l" inf-clojure-load-file "file")
      ("g" inf-clojure-reload "reload"))
     "Eval"
     (("s" inf-clojure-set-ns "set-repl-ns")
      ("e" inf-clojure-eval-last-sexp "eval-last")
      ("f" inf-clojure-eval-defun "eval-defun")
      ("x" inf-clojure-macroexpand "expand"))
     "Find"
     (("d" inf-clojure-show-var-documentation "doc")
      ("a" inf-clojure-apropos "apropos"))))

  :commands (inf-clojure-minor-mode
             inf-clojure-connect
             inf-clojure))

(use-package flycheck-joker
  :straight t)

(use-package flycheck-clj-kondo
  :straight t
  :after (flycheck-joker)
  :config (dolist (checkers '((clj-kondo-clj . clojure-joker)
                              (clj-kondo-cljs . clojurescript-joker)
                              (clj-kondo-cljc . clojure-joker)))
            (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers)))))

;; (use-package clj-refactor
;;   :straight t
;;   :commands (hydra-cljr-cljr-menu/body
;;              hydra-cljr-ns-menu/body
;;              hydra-cljr-code-menu/body
;;              hydra-cljr-project-menu/body
;;              hydra-cljr-toplevel-form-menu/body)
;;   :config
;;   (progn
;;     (add-hook 'cider-mode-hook (lambda ()
;;                                  (clj-refactor-mode 1)))

;;     (major-mode-hydra-bind clojure-mode "Refactor"
;;       ("rs" hydra-cljr-cljr-menu/body "refactor")
;;       ("rn" hydra-cljr-ns-menu/body "namespaces")
;;       ("rc" hydra-cljr-code-menu/body "code")
;;       ("rp" hydra-cljr-project-menu/body "project")
;;       ("rt" hydra-cljr-toplevel-form-menu/body "top level form"))))

(provide 'jp-clojure)
;;; jp-clojure.el ends here
