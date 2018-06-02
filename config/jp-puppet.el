;;; jp-puppet.el --- Puppet configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package puppet-mode
  :straight t
  :mode (("\\.pp\\'"  . puppet-mode)))

(provide 'jp-puppet)
;;; jp-puppet.el ends here
