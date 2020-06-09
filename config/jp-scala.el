;;; jp-scala.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package scala-mode
  :straight t
  :mode ("\\.scala\\'" "\\.sbt\\'")
  :defer t
  :init
  (progn
    (dolist (ext '(".cfe" ".cfs" ".si" ".gen" ".lock"))
      (add-to-list 'completion-ignored-extensions ext)))
  :config
  (progn
    (require 'jp-scala-editing)
    (add-hook 'scala-mode-hook
              (lambda ()
                ;; Add the hook to the end of the list and use buffer local hooks
                ;; because we want to enable this hook only in scala-mode.
                (add-hook 'post-command-hook 'jp-scala-insert-margin-befor-ending-parens t t)))

    (define-key scala-mode-map (kbd "RET") 'jp-scala-newline-and-indent-with-pipe)
    ;;(define-key scala-mode-map (kbd ">") 'jp-scala-gt)
    ;;(define-key scala-mode-map (kbd "-") 'jp-scala-hyphen)
    (define-key scala-mode-map (kbd "M-j") 'jp-scala-join-line)

    ;; Compatibility with `aggressive-indent'
    (setq scala-indent:align-forms nil
          scala-indent:align-parameters nil
          scala-indent:default-run-on-strategy scala-indent:operator-strategy)))

(use-package sbt-mode
  :straight t
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package bloop
  :preface
  (progn
    (defvar jp-scala-hydra--title)
    (setq jp-scala-hydra--title  (with-octicon "server" "Scala Language Server Commands")))
  :straight (:host github :type git :repo "tarao/emacs-bloop")
  :after (lsp-mode)
  :mode-hydra
  (scala-mode
   (:color teal :quit-key "q" :title jp-scala-hydra--title)
   ("Build & Test"
    (("t" bloop-test-only "test")
     ("T" bloop-test "rename")
     ("b" bloop-compile "compile")))))

(use-package ammonite-term-repl
  :straight t
  :commands (run-ammonite)
  :custom
  (ammonite-term-repl-auto-detect-predef-file nil)
  (ammonite-term-repl-program-args '("--class-based")))

(use-package ob-ammonite
  :straight t)

(provide 'jp-scala)
;;; jp-scala.el ends here
