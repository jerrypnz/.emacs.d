;;; jp-projectile-utils.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(autoload 'counsel-rg "counsel")
(autoload 'counsel-projectile-switch-to-buffer "counsel-projectile")
(autoload 'projectile-project-p "projectile")

(defun jp-open-file (current-dir-p)
  (interactive "P")
  (if (and (projectile-project-p)
           (not current-dir-p))
      (counsel-projectile-find-file)
    (counsel-find-file)))

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
    (counsel-rg "" default-directory "" (format "[%s] rg" default-directory))))

(defvar counsel-projectile-rg-initial-input nil) ;; defined in counsel-projectile.el

(defun jp-search-symbol-at-pt (current-dir-p)
  (interactive "P")
  (if (and (projectile-project-p)
           (not current-dir-p))
      (let ((counsel-projectile-rg-initial-input '(thing-at-point 'symbol t)))
        (counsel-projectile-rg))
    (let ((sym (thing-at-point 'symbol t)))
      (counsel-rg sym default-directory "" (format "[%s] rg" default-directory)))))

(defun jp-refresh-projectile-projects ()
  (interactive)
  (when (require 'magit nil t)
        (projectile-cleanup-known-projects)
        (->> (magit-list-repos)
             (-map #'file-name-as-directory)
             (mapc #'projectile-add-known-project))))

(provide 'jp-projectile-utils)
;;; jp-projectile-utils.el ends here
