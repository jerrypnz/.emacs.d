;;; jp-projectile-utils.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'consult)
(require 'projectile)

(defun jp-open-file (current-dir-p)
  (interactive "P")
  (if (and (projectile-project-p)
           (not current-dir-p))
      (call-interactively #'projectile-find-file)
    (call-interactively #'find-file)))

(defun jp-switch-buffer (global-p)
  (interactive "P")
  (if (and (projectile-project-p)
           (not global-p))
      (call-interactively #'projectile-switch-to-buffer)
    (call-interactively #'consult-buffer)))

(defun jp-search (current-dir-p)
  (interactive "P")
  (if (and (projectile-project-p)
           (not current-dir-p))
      (consult-ripgrep (projectile-project-root))
    (consult-ripgrep default-directory)))

(defun jp-search-symbol-at-pt (current-dir-p)
  (interactive "P")
  (if (and (projectile-project-p)
           (not current-dir-p))
      (consult-ripgrep (projectile-project-root) (thing-at-point 'symbol t))
    (consult-ripgrep default-directory (thing-at-point 'symbol t))))

(defun jp-refresh-projectile-projects ()
  (interactive)
  (when (require 'magit nil t)
        (projectile-cleanup-known-projects)
        (->> (magit-list-repos)
             (-map #'file-name-as-directory)
             (mapc #'projectile-add-known-project))))

(provide 'jp-projectile-utils)
;;; jp-projectile-utils.el ends here
