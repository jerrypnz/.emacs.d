;;; jp-major-mode-hydra.el --- Major mode keybindings managed by Hydra -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'jp-fancy-hydra)

(defvar jp-major-mode-hydra--heads-alist nil
  "An alist holding hydra heads for each major mode, keyed by the mode name.")

(defvar jp-major-mode-hydra--body-cache nil
  "An alist holding compiled hydras for each major mode. Whenever
  `jp-major-mode-hydra--heads-alist' is changed, the hydra for
  the mode gets recompiled.")

;;TODO Remove me
(setq jp-major-mode-hydra--heads-alist
      '((emacs-lisp-mode . ("Test Emacs"      (("v" emacs-version "Emacs Version"))))
        (restclient-mode . ("Test RestClient" (("v" emacs-version "Emacs Version"))))))

(defun jp-major-mode-hydra-recompile (mode heads-plist)
  (let ((hydra-name (make-symbol (format "jp-major-mode-hydras/%s" mode)))
        ;; By default, exit hydra after invoking a head and warn if a foreign key is pressed.
        (hydra-body '(:exit t :hint nil :foreign-keys warn)))
    (eval `(defancyhydra ,hydra-name ,hydra-body ,heads-plist))))

(defun jp-major-mode-hydra-get-or-recompile (mode)
  (-if-let (hydra (alist-get mode jp-major-mode-hydra--body-cache))
      hydra
    (-when-let (heads-plist (alist-get mode jp-major-mode-hydra--heads-alist))
      (let ((hydra (jp-major-mode-hydra-recompile mode heads-plist)))
        (setf (alist-get mode jp-major-mode-hydra--body-cache) hydra)
        hydra))))

(defun jp-major-mode-hydra ()
  (interactive)
  (let* ((mode major-mode)
         (hydra (jp-major-mode-hydra-get-or-recompile mode)))
    (if hydra
        (call-interactively hydra)
      (message "Major mode hydra not found for %s" mode))))

;; TODO Remove me
(jp-major-mode-hydra-get-or-recompile 'restclient-mode)

(provide 'jp-major-mode-hydra)

;;; jp-major-mode-hydra.el ends here
