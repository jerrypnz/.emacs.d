;;; jp-lua.el --- Lua/Fennel configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

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

(provide 'jp-lua)
;;; jp-lua.el ends here
