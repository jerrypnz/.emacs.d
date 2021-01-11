;;; jp-lsp.el --- LSP related config -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package lsp-mode
  ;; :straight (:host github :repo "emacs-lsp/lsp-mode"
  ;;            :fork (:host github :repo "jerrypnz/lsp-mode"))
  :straight t
  :hook (((go-mode python-mode rust-mode java-mode scala-mode clojure-mode) . lsp-deferred)
         (lsp-mode . lsp-lens-mode))

  :bind
  (:map lsp-mode-map
   ("M-." . lsp-find-definition))

  :config
  (setq lsp-auto-execute-action nil)
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil))

(use-package lsp-treemacs
  :straight t
  :after (lsp-mode)
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-clojure
  :after (lsp-mode))

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
  :straight t
  :after (lsp-mode)
  :config
  (setq lsp-java-server-install-dir (expand-file-name "~/.jdt.ls/")
        lsp-java-workspace-dir (expand-file-name "~/.jdt-workspace/")
        lsp-java-workspace-cache-dir (expand-file-name ".cache/" lsp-java-workspace-dir)
        lsp-java-maven-download-sources t
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-format-settings-profile "GoogleStyle"))

(use-package dap-java
  :after (dap-mode)
  :config
  (setq dap-java-test-runner (expand-file-name "~/.jdt.ls/test-runner/junit-platform-console-standalone.jar")))

(use-package lsp-rust
  :after (lsp-mode)
  :init
  (progn
    (defvar lsp-rust-server)
    (setq lsp-rust-server 'rust-analyzer))
  :custom
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-max-inlay-hint-length 16)
  (lsp-rust-analyzer-use-client-watching t)
  (lsp-rust-analyzer-cargo-watch-enable t)
  (lsp-rust-analyzer-cargo-watch-command "clippy"))

(use-package lsp-metals
  :straight t
  :after (lsp-mode)
  :custom
  (lsp-metals-maven-script "/usr/bin/mvn")
  (lsp-metals-sbt-script "/usr/bin/sbt")
  ;;(lsp-metals-bloop-version "1.4.2")
  )

;; (use-package lsp-python-ms
;;   :straight t
;;   :after (lsp-mode)
;;   :config
;;   (setq lsp-python-ms-auto-install-server t))

(use-package lsp-pyright
  :straight t
  :after (lsp-mode))

(use-package lsp-ivy
  :straight (:host github :repo "emacs-lsp/lsp-ivy" :branch "master"))

(use-package jp-lsp-hydra)

(provide 'jp-lsp)
;;; jp-lsp.el ends here
