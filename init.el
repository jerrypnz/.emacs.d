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

(when (boundp 'comp-deferred-compilation)
  (setq comp-deferred-compilation nil))

(defvar jp-emacs-cache-dir (expand-file-name ".emacs.d.cache/" "~/"))
(defvar jp-emacs-eln-cache-dir (expand-file-name "eln-cache" jp-emacs-cache-dir))
(defvar jp-emacs-undo-history-dir (expand-file-name "undo" jp-emacs-cache-dir))
(make-directory jp-emacs-eln-cache-dir t)
(make-directory jp-emacs-undo-history-dir t)

(defvar jp-straight-profile (expand-file-name "straight-pkgs.el" user-emacs-directory))

(defvar straight-base-dir)
(defvar straight-profiles)
(setq straight-base-dir jp-emacs-cache-dir)
(setq straight-profiles `((nil . ,jp-straight-profile)))

(defvar native-comp-eln-load-path)
(setq native-comp-eln-load-path (cons jp-emacs-eln-cache-dir (cdr native-comp-eln-load-path)))

;; Bootstrap straight.el package manager.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" jp-emacs-cache-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-disable-native-compile nil)
(setq native-comp-enable-subr-trampolines nil)

;; Install some basic packages

(straight-use-package 'dash)
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
(use-package jp-hydra)
(use-package jp-org)
(use-package jp-ui)
(use-package jp-base)
(use-package jp-smartparens)
(use-package jp-completion)
(use-package jp-flycheck)
(use-package jp-projectile)
(use-package jp-eyebrowse)
(use-package jp-magit)
(use-package jp-restclient)
(use-package jp-yasnippet)
(use-package jp-treemacs)
(use-package jp-bufler)
(use-package jp-elisp)

;; Programming language support
(use-package jp-prog)

;; LLM
(use-package jp-llm)

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
