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
  :preface
  (progn
    (defvar cargo-hydra--title)
    (setq cargo-hydra--title (with-octicon "package" "Cargo")))
  :straight t

  :pretty-hydra
  ((:title cargo-hydra--title :quit-key "q")
   ("Packages"
    (("a" cargo-process-add "add")
     ("u" cargo-process-update "update")
     ("x" cargo-process-rm "remove")
     ("U" cargo-process-upgrade "upgrade"))
    "Actions"
    (("b" cargo-process-build "build")
     ("r" cargo-process-run "run")
     ("c" cargo-process-check "check")
     ("f" cargo-process-format "format")
     ("s" cargo-process-search "search"))
    "Tests"
    (("t" cargo-process-current-file-tests "test")
     ("T" cargo-process-test "test all"))))

  :mode-hydra
  (rust-mode
   ("Cargo"
    (("b" cargo-process-build "build")
     ("r" cargo-process-run "run")
     ("t" cargo-process-current-file-tests "test")
     ("T" cargo-process-test "test all")
     ("c" cargo-hydra/body "more...")))))

(provide 'jp-rust)
;;; jp-rust.el ends here
