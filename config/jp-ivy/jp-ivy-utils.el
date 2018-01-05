;;; jp-ivy-utils.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(autoload 'counsel-grep-or-swiper "counsel")

(require 'thingatpt)

(defun jp-counsel-grep-or-swiper-symbol-at-pt ()
  (interactive)
  (let ((sym (thing-at-point 'symbol t)))
    (counsel-grep-or-swiper sym)))

(provide 'jp-ivy-utils)
;;; jp-ivy-utils.el ends here
