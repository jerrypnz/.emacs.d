;;; jp-lsp.el --- LSP related config -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package lsp-mode
  :straight t
  :preface
  (progn
    (defvar jp-lsp-hydra--title)
    (setq jp-lsp-hydra--title  (with-octicon "server" "Language Server Commands")))

  :hook ((go-mode rust-mode java-mode scala-mode) . lsp-deferred)

  :pretty-hydra
  ((:color teal :quit-key "q" :title jp-lsp-hydra--title)
   ("Connection"
    (("cc" lsp "start")
     ("cr" lsp-restart-workspace "restart")
     ("cd" lsp-describe-session "describe session")
     ("cq" lsp-disconnect "disconnect"))
    "Find & Goto"
    (("d" lsp-describe-thing-at-point "describe symbol"))
    "Refactor"
    (("rr" lsp-rename "rename")
     ("ra" lsp-execute-code-action "code action")
     ("rf" lsp-format-buffer "format"))
    "Toggles"
    (("tl" lsp-lens-mode "toggle lens" :toggle t :exit nil))))

  :mode-hydra
  ((rust-mode go-mode java-mode scala-mode)
   (:color teal :quit-key "q" :title jp-lsp-hydra--title)
   ("LSP"
    (("d" lsp-describe-thing-at-point "describe")
     ("R" lsp-rename "rename")
     ("A" lsp-execute-code-action "code action")
     ("F" lsp-format-buffer "format")
     ("l" lsp-mode-hydra/body "more..."))))

  :config
  (setq lsp-auto-execute-action nil))

(use-package lsp-clients
  :after (lsp-mode))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)

  :bind
  (:map lsp-ui-mode-map
   ("M-." . lsp-ui-peek-find-definitions)
   ("M-?" . lsp-ui-peek-find-references))

  :pretty-hydra
  (lsp-mode-hydra
   ("Toggles"
    (("td" lsp-ui-doc-mode "toggle hover doc" :toggle t :exit nil)
     ("ts" lsp-ui-sideline-mode "toggle sideline" :toggle t :exit nil))
    "Find & Goto"
    (("gr" lsp-ui-peek-find-references "references")
     ("gd" lsp-ui-peek-find-definitions "definitions"))))

  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil))

(use-package dap-mode
  :straight t
  :after (lsp-mode)
  :hook ((lsp-mode . dap-mode)
         (lsp-mode . dap-ui-mode))
  :config
  (progn
    (require 'ansi-color)
    (defun colorize-compilation-buffer ()
      (toggle-read-only)
      (ansi-color-apply-on-region compilation-filter-start (point))
      (toggle-read-only))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))

(use-package lsp-java
  :preface
  (progn
    (defvar jp-java-hydra--title)
    (setq jp-java-hydra--title  (with-octicon "server" "Java Language Server Commands")))
  :straight t
  :after (lsp-mode)
  :mode-hydra
  ((java-mode)
   (:color teal :quit-key "q" :title jp-java-hydra--title)
   ("Action"
    (("b" lsp-java-build-project "build")
     ("o" lsp-java-organize-imports "organize imports")
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
  :config
  (setq lsp-java-server-install-dir (expand-file-name "~/.jdt.ls/")
        lsp-java-workspace-dir (expand-file-name "~/.jdt-workspace/")
        lsp-java-workspace-cache-dir (expand-file-name ".cache/" lsp-java-workspace-dir)
        lsp-java-maven-download-sources t))

(use-package dap-java
  :after (dap-mode)
  :mode-hydra
  ((java-mode)
   nil
   ("Run/Debug"
    (("t" dap-java-run-test-class "run test class")
     ("T" dap-java-debug-test-class "debug test class"))))
  :config
  (setq dap-java-test-runner (expand-file-name "~/.jdt.ls/test-runner/junit-platform-console-standalone.jar")))

(use-package lsp-rust
  :after (lsp-mode)
  :init
  (progn
    (defvar lsp-rust-analyzer-server-display-inlay-hints)
    (setq lsp-rust-analyzer-server-display-inlay-hints t)
    (defvar lsp-rust-server)
    (setq lsp-rust-server 'rust-analyzer))
  :config
  (progn
    (setq lsp-rust-analyzer-max-inlay-hint-length 16)
    (setq lsp-rust-analyzer-use-client-watching t)
    (setq lsp-rust-analyzer-cargo-watch-enable t)
    (setq lsp-rust-analyzer-cargo-watch-command "clippy")))

(use-package lsp-metals
  :after (lsp-mode))

(use-package lsp-ivy
  :straight (:host github :repo "emacs-lsp/lsp-ivy" :branch "master")

  :pretty-hydra
  (lsp-mode-hydra
   ("Find"
    (("gf" lsp-ivy-workspace-symbol "workspace symbol"))))

  :mode-hydra
  ((rust-mode go-mode java-mode) nil
   ("LSP"
    (("a" lsp-ivy-workspace-symbol "workspace symbol")))))

(provide 'jp-lsp)
;;; jp-lsp.el ends here
