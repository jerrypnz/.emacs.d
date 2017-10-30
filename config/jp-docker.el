;;; jp-docker.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(provide 'jp-docker)
;;; jp-docker.el ends here
