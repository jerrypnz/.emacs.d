;;; jp-yaml.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(provide 'jp-yaml)
;;; jp-yaml.el ends here
