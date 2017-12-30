;;; jp-projectile-utils.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(autoload 'counsel-rg "counsel")
(autoload 'counsel-projectile-switch-to-buffer "counsel-projectile")
(autoload 'projectile-project-p "projectile")

(defun jp-switch-buffer (global-p)
  (interactive "P")
  (if (and (projectile-project-p)
           (not global-p))
      (counsel-projectile-switch-to-buffer)
    (ivy-switch-buffer)))

(defun jp-search (current-dir-p)
  (interactive "P")
  (if (and (projectile-project-p)
           (not current-dir-p))
      (counsel-projectile-rg)
    (counsel-rg "" default-directory)))

(provide 'jp-projectile-utils)
;;; jp-projectile-utils.el ends here
