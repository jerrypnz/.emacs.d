;;; jp-completion-utils.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(defun jp-consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol t)))

(provide 'jp-completion-utils)
;;; jp-completion-utils.el ends here
