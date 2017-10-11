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

(setq gc-cons-threshold (* 800 1024))

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Initialize package.el
;;
;; Most packages are installed using git subtrees, but some packages (such as
;; flycheck) break unless installed via package.el.

(require 'package)
(add-to-list 'package-archives '("MELPA" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package.

(require 'seq)
(require 'subr-x)

(defun jp-init/init-load-path (&optional interactive-p)
  "Add select subdirs of `user-emacs-directory' to the `load-path'.

If argument INTERACTIVE-P is set, log additional information."
  (interactive "p")
  (let* ((before load-path)
         (lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (config-dir (expand-file-name "config" user-emacs-directory))
         (git-subtrees
          (seq-filter #'file-directory-p
                      (directory-files lisp-dir t "^[^.]")))
         (config-subtrees
          (seq-filter #'file-directory-p
                      (directory-files config-dir t "^[^.]"))))
    (dolist (path (append (list lisp-dir config-dir) config-subtrees git-subtrees))
      (add-to-list 'load-path path)
      (add-to-list 'Info-default-directory-list path)
      (add-to-list 'load-path (concat path "/emacs"))
      (add-to-list 'load-path (concat path "/elisp"))
      (add-to-list 'load-path (concat path "/lisp")))

    (setq load-path (seq-filter #'file-directory-p load-path))
    (setq Info-default-directory-list (seq-filter #'file-directory-p Info-default-directory-list))

    (when interactive-p
      (if-let (added (seq-difference load-path before))
          (message "Load path updated. Added: %S" added)
        (message "No change to load-path")))))

(jp-init/init-load-path)

(defconst use-package-verbose t)
(require 'use-package)


;; Load features.
(use-package jp-base)
(use-package jp-smartparens)
(use-package jp-ivy)
(use-package jp-company)
(use-package jp-flycheck)
(use-package jp-projectile)
(use-package jp-eyebrowse)
(use-package jp-magit)
(use-package jp-go)

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


(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7ef2884658a1fed818a11854c232511fa25721c60083a2695e6ea34ce14777ee" default)))
 '(package-selected-packages (quote (flycheck))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
