;;; jp-lsp.el --- LSP related config -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package lsp-mode
  :straight t
  :demand t
  :preface
  (progn
    (defvar jp-lsp-hydra--title)
    (setq jp-lsp-hydra--title  (with-octicon "server" "Language Server Commands")))

  :hook ((go-mode rust-mode) . lsp-deferred)

  :pretty-hydra
  ((:color teal :quit-key "q" :title jp-lsp-hydra--title)
   ("Connection"
    (("c" lsp "start")
     ("R" lsp-restart-workspace "restart")
     ("D" lsp-describe-session "describe session")
     ("Q" lsp-disconnect "disconnect"))
    "Navigation"
    (("d" lsp-describe-thing-at-point "describe symbol")
     ("R" lsp-find-references "references")
     ("j" lsp-find-definition "definition")
     ("t" lsp-goto-type-definition "goto type def")
     ("i" lsp-goto-implementation "goto impl"))
    "Refactor"
    (("r" lsp-rename "rename")
     ("f" lsp-format-buffer "format"))
    "Toggles"
    (("l" lsp-lens-mode "toggle lens" :toggle t :exit nil))))

  :bind
  (:map lsp-mode-map
   ("C-M-SPC" . lsp-mode-hydra/body)
   ("M-." . lsp-find-definition))

  :config
  (require 'lsp-clients)
  (setq lsp-prefer-flymake nil)
  (setq lsp-auto-configure nil))

(use-package lsp-ui
  :straight t
  :after (lsp-mode)
  :pretty-hydra
  (lsp-mode-hydra
   ("Toggles"
    (("u" lsp-ui-mode "toggle ui" :toggle t :exit nil)))))

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
