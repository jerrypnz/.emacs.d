;;; jp-protobuf.el --- Protobuf configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package protobuf-mode
  :straight (:host github :repo "emacsmirror/protobuf-mode")
  :mode ("\\.proto\\'"))

(provide 'jp-protobuf)
;;; jp-protobuf.el ends here
