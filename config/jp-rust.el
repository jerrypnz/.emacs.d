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

;;;; TODO Not ready yet, check it out later
;; (use-package ra-emacs-lsp
;;   :straight (:host github
;;              :repo "rust-analyzer/rust-analyzer"
;;              :branch "master"
;;              :files ("editors/emacs/*.el")))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :preface
  (progn
    (defvar cargo-hydra--title)
    (setq cargo-hydra--title (with-octicon "package" "Cargo")))
  :straight t

  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Cargo")
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.25)))

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

(use-package flycheck-rust
  :straight t
  :hook (flycheck-mode . flycheck-rust-setup))

(provide 'jp-rust)
;;; jp-rust.el ends here
