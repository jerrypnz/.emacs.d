;;; jp-icons.el --- all-the-icons helper functions -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'all-the-icons)

(defun with-faicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-fileicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-octicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-material (icon str &optional height v-adjust)
  (s-concat (all-the-icons-material icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-mode-icon (mode str &optional height nospace)
  (let* ((v-adjust (if (eq major-mode 'emacs-lisp-mode) -0.1 0.05))
         (icon     (all-the-icons-icon-for-mode mode
                                                :height (or height 1)
                                                :v-adjust v-adjust))
         (icon     (if (symbolp icon)
                       (all-the-icons-icon-for-mode 'fundamental-mode
                                                    :height (or height 1)
                                                    :v-adjust v-adjust)
                     icon)))
    (s-concat icon (if nospace "" " ") str)))

(provide 'jp-icons)

;;; jp-icons.el ends here
