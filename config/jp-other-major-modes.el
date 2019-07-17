;;; jp-other-major-modes.el --- Other (unimportant) major mode configs -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package protobuf-mode
  :straight (:host github :repo "emacsmirror/protobuf-mode")
  :mode ("\\.proto\\'"))

(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'")

(use-package puppet-mode
  :straight t
  :mode (("\\.pp\\'"  . puppet-mode)))

(use-package php-mode
  :straight t
  :mode ("\\.php\\'"))

(use-package yaml-mode
  :straight t
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(use-package cql-mode
  :straight t
  :mode ("\\.cql\\'" "\\.hcql\\'"))

(use-package lua-mode
  :straight (:host github :repo "immerrr/lua-mode")
  :mode ("\\.lua\\'")
  :interpreter "lua")

(use-package fennel-mode
  :straight (:host gitlab :repo "technomancy/fennel-mode")
  :mode ("\\.fnl\\'")
  :config
  (progn
    (put 'global 'fennel-indent-function 1)
    (put 'if 'fennel-indent-function 1)))

(provide 'jp-other-major-modes)
;;; jp-other-major-modes.el ends here
