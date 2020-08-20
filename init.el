;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;; The init system and the config structure are based on Chris
;; Barrett's amazing work: https://github.com/chrisbarrett/.emacs.d

;; Declares some variables and bootstraps the rest of the configuration.
;;
;; One main difference from other configurations out there is that I use git subtrees for
;; many core packages, instead of relying on the Emacs package manager.

;;; Code:

(setq gc-cons-threshold 100000000)

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Bootstrap straight.el package manager.

(eval-and-compile
  (defvar bootstrap-version 3)
  (defvar bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el")))

(unless (file-exists-p bootstrap-file)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))

(with-no-warnings
  (setq straight-cache-autoloads t)
  (setq straight-check-for-modifications 'live))

(require 'straight bootstrap-file t)

;; Install some basic packages

(straight-use-package 'dash)
(straight-use-package 'dash-functional)
(straight-use-package 'f)
(straight-use-package 's)
(straight-use-package 'noflet)
(straight-use-package 'memoize)
(straight-use-package 'el-patch)

(with-no-warnings
  (setq use-package-verbose t))

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

(require 'seq)
(require 'subr-x)

(defun jp-init/init-load-path (&optional interactive-p)
  (interactive "p")
  (let* ((before load-path)
         (lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (config-dir (expand-file-name "config" user-emacs-directory)))
    (dolist (path (list lisp-dir config-dir))
      (add-to-list 'load-path path)
      (add-to-list 'Info-default-directory-list path))

    (setq load-path (seq-filter #'file-directory-p load-path))
    (setq Info-default-directory-list (seq-filter #'file-directory-p Info-default-directory-list))

    (when interactive-p
      (if-let (added (seq-difference load-path before))
          (message "Load path updated. Added: %S" added)
        (message "No change to load-path")))))

(jp-init/init-load-path)

;; Load features.
(use-package jp-basic-settings)
(use-package jp-org)
(use-package jp-ui)
(use-package jp-base)
(use-package jp-smartparens)
(use-package jp-ivy)
(use-package jp-company)
(use-package jp-flycheck)
(use-package jp-projectile)
(use-package jp-eyebrowse)
(use-package jp-magit)
(use-package jp-restclient)
(use-package jp-yasnippet)
(use-package jp-treemacs)
(use-package jp-bufler)

;; Programming language support
(use-package jp-clojure)
(use-package jp-scala)
(use-package jp-elisp)
(use-package jp-lsp)
(use-package jp-rust)
(use-package jp-go)

;; Other major modes
(use-package jp-markdown)
(use-package jp-plantuml)
(use-package jp-other-major-modes)

;; Tools
(use-package jp-elfeed)
(use-package jp-org-roam)
(use-package jp-org-journal)

;;; Print overall startup time.

(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(provide 'init)

;;; init.el ends here
