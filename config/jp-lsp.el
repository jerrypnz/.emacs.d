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

  :hook ((go-mode rust-mode) . lsp-deferred)

  :pretty-hydra
  ((:color teal :quit-key "q" :title jp-lsp-hydra--title)
   ("Connection"
    (("cc" lsp "start")
     ("cr" lsp-restart-workspace "restart")
     ("cd" lsp-describe-session "describe session")
     ("cq" lsp-disconnect "disconnect"))
    "Find & Goto"
    (("d" lsp-describe-thing-at-point "describe symbol")
     ("gr" lsp-find-references "references")
     ("gd" lsp-find-definition "definition")
     ("gt" lsp-goto-type-definition "goto type def")
     ("gi" lsp-goto-implementation "goto impl"))
    "Refactor"
    (("rr" lsp-rename "rename")
     ("rf" lsp-format-buffer "format"))
    "Toggles"
    (("tl" lsp-lens-mode "toggle lens" :toggle t :exit nil))))

  :mode-hydra
  ((rust-mode go-mode java-mode)
   (:color teal :quit-key "q" :title jp-lsp-hydra--title)
   ("LSP"
    (("d" lsp-describe-thing-at-point "describe")
     ("R" lsp-rename "rename")
     ("F" lsp-format-buffer "format")
     ("l" lsp-mode-hydra/body "more..."))))

  :config
  (require 'lsp-clients)
  (setq lsp-prefer-flymake nil)
  (setq lsp-auto-configure nil))

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
    (("td" lsp-ui-doc-mode "toggle hover doc" :toggle t :exit nil))))

  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil))

(use-package lsp-java
  :straight t
  :after (lsp-mode)
  :hook (java-mode . lsp-deferred)
  :config
  (setq lsp-java-server-install-dir "~/.jdt.ls/"
        lsp-java-workspace-dir "~/.jdt-workspace/"
        lsp-java-workspace-cache-dir (expand-file-name ".cache/" lsp-java-workspace-dir)
        lsp-java-maven-download-sources t))

(use-package company-lsp
  :straight t
  :after (lsp-mode company)
  :config
  (add-to-list 'company-backends 'company-lsp))

(provide 'jp-lsp)
;;; jp-lsp.el ends here
