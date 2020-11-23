;;; jp-lsp-hydra.el --- Hydra for LSP modes -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'major-mode-hydra)

(defun jp-lsp-hydra--title ()
  (with-mode-icon major-mode
                  (propertize (s-concat "LSP Commands (" (format-mode-line mode-name) ")")
                              'face '(:weight bold :height 1.1))
                  1.1))

(major-mode-hydra-define+ (rust-mode go-mode python-mode java-mode scala-mode)
  (:color teal :quit-key "q" :title (jp-lsp-hydra--title))
  ("Quick Action"
   (("d" lsp-describe-thing-at-point "describe symbol")
    ("a" lsp-execute-code-action "code action")
    ("f" lsp-format-buffer "format")
    ("O" lsp-organize-imports "organize imports"))
   "Find & Goto"
   (("gr" lsp-ui-peek-find-references "references")
    ("gd" lsp-ui-peek-find-definitions "definitions")
    ("gf" lsp-ivy-workspace-symbol "workspace symbol"))
   "Connection"
   (("cc" lsp "start")
    ("cr" lsp-restart-workspace "restart")
    ("cd" lsp-describe-session "describe session")
    ("cq" lsp-shutdown-workspace "shutdown"))
   "Toggles"
   (("ol" lsp-lens-mode "toggle lens" :toggle t :exit nil)
    ("od" lsp-ui-doc-mode "toggle hover doc" :toggle t :exit nil)
    ("os" lsp-ui-sideline-mode "toggle sideline" :toggle t :exit nil))))

(major-mode-hydra-define+ java-mode nil
  ("Quick Action"
   (("b" lsp-java-build-project "build")
    ("t" dap-java-run-test-class "test")
    ("T" dap-java-debug-test-class "debug test")
    ("U" lsp-java-update-project-configuration "update project config"))
   "Generate"
   (("gs" lsp-generate-to-string "generate toString")
    ("ge" lsp-java-generate-equals-and-hash-code "generate equals/hashCode")
    ("go" lsp-java-generate-overrides "generate overrides")
    ("gg" lsp-java-generate-getters-and-setters "generate getters/setters"))
   "Refactoring"
   (("re" lsp-java-extract-to-constant "extract constant")
    ("rm" lsp-java-extract-method "extract method")
    ("ri" lsp-java-add-import  "add import")
    ("ru" lsp-java-add-unimplemented-methods "add unimplemented methods")
    ("rp" lsp-java-create-parameter "introduce parameter")
    ("rf" lsp-java-create-field "introduce field")
    ("rl" lsp-java-create-local "introduce local variable"))))

(defun jp-lsp-metals-build-restart ()
  (interactive)
  (lsp-send-execute-command "build-restart" ()))

(major-mode-hydra-define+ scala-mode nil
  ("Connection"
   (("ci" lsp-metals-build-import "import build")
    ("cg" jp-lsp-metals-build-restart "restart build"))))

(provide 'jp-lsp-hydra)

;;; jp-lsp-hydra.el ends here
