;;; jp-php.el --- PHP configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package php-mode
  :straight t
  :mode ("\\.php\\'"))

(provide 'jp-php)
;;; jp-php.el ends here
