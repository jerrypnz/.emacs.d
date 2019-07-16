;;; jp-rust.el --- Rust configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package toml-mode
  :straight t
  :mode "\\.toml\\'")

(use-package rust-mode
  :straight t
  :mode "\\.rs\\'")

;; Add keybindings for interacting with Cargo
(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :straight t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'jp-rust)
;;; jp-rust.el ends here
