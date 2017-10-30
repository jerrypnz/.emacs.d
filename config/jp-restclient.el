;;; jp-restclient.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package restclient
  :commands (restclient-mode))

(provide 'jp-restclient)
;;; jp-restclient.el ends here
