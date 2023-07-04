;;; jp-magit.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; magit
(use-package magit
  :straight t
  :defer 2
  :init
  ;; Since we're using git subtree without building it with Make, we
  ;; don't have "magit-version.el" file.  Get around it by setting the
  ;; version here explicitly.
  (setq magit-version "2.11.0")
  (defvar jp-git--title)
  (setq jp-git--title (with-octicon "git-compare" "Git"))

  :pretty-hydra
  (jp-git
   (:color teal :quit-key "q" :title jp-git--title)
   ("Magit"
    (("s" magit-status "magit status")
     ("l" magit-log-buffer-file "commit log (current file)")
     ("L" magit-log-current "commit log (project)")
     ("b" magit-blame-addition "blame"))
    "Diff"
    (("d" magit-diff-buffer-file "diff buffer"))))

  :config
  (setq magit-repository-directories
        '(("~/dev/workspace/"   . 3)
          ("~/dev/personal/"    . 2)
          ("~/dev/open-source/" . 2)))

  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function #'magit-restore-window-configuration)

  (transient-bind-q-to-quit))

;; (use-package magit-todos
;;   :straight t
;;   :after (magit)
;;   :config
;;   (progn
;;     (setq magit-todos-require-colon nil)
;;     (magit-todos-mode)))

;; git-timemachine
(use-package git-timemachine
  :straight (:host github :repo "emacsmirror/git-timemachine")
  :pretty-hydra
  (jp-git
   ("Other"
    (("t" git-timemachine "time machine"))))
  :commands (git-timemachine))

;; diff-hl
(use-package diff-hl
  :straight t
  :defer nil
  :pretty-hydra
  (jp-git
   ("Diff"
    (("n" diff-hl-next-hunk "next hunk" :exit nil)
     ("p" diff-hl-previous-hunk "next hunk" :exit nil)
     ("V" diff-hl-revert-hunk "revert hunk" :exit nil))))

  :config
  (progn
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (global-diff-hl-mode)
    ;;(diff-hl-flydiff-mode) ;; It makes magit really slow
    ))

(provide 'jp-magit)
;;; jp-magit.el ends here
