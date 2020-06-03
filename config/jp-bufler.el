;;; jp-bufler.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package bufler
  :straight t
  :bind (("M-i" . bufler-switch-buffer)
         ("C-x b" . bufler-switch-buffer)
         ("C-x C-b" . bufler))
  :config
  (setq bufler-groups
        (bufler-defgroups
          (group
           ;; Subgroup collecting all named workspaces.
           (auto-workspace))
          (group
           ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
           (group-or "*Help/Info*"
                     (mode-match "*Help*" (rx bos "help-"))
                     (mode-match "*Help*" (rx bos "helpful-"))
                     (mode-match "*Info*" (rx bos "info-"))))
          (group
           ;; Subgroup collecting all special buffers (i.e. ones that are not
           ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
           ;; through to other groups, so they end up grouped with their project buffers).
           (group-and "*Special*"
                      (lambda (buffer)
                        (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                             buffer)
                                    (funcall (mode-match "Dired" (rx bos "dired"))
                                             buffer)
                                    (funcall (auto-file) buffer))
                          "*Special*")))
           (group
            ;; Subgroup collecting these "special special" buffers
            ;; separately for convenience.
            (name-match "**Emacs**"
                        (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
           (group
            ;; Subgroup collecting all other Magit buffers, grouped by directory.
            (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
            (auto-directory))
           ;; Remaining special buffers are grouped automatically by mode.
           (auto-mode))
          ;; All buffers under "~/.emacs.d" (or wherever it is).
          (dir user-emacs-directory)
          (group
           ;; Subgroup collecting buffers in `org-directory' (or "~/org" if
           ;; `org-directory' is not yet defined).
           (dir (if (bound-and-true-p org-directory)
                    org-directory
                  "~/org"))
           (group
            ;; Subgroup collecting indirect Org buffers, grouping them by file.
            ;; This is very useful when used with `org-tree-to-indirect-buffer'.
            (auto-indirect)
            (auto-file))
           ;; Group remaining buffers by whether they're file backed, then by mode.
           (group-not "*special*" (auto-file))
           (auto-mode))
          (group
           ;; Subgroup collecting buffers in a projectile project.
           (auto-projectile))
          (group
           ;; Subgroup collecting buffers in a version-control project,
           ;; grouping them by directory.
           (auto-project))
          ;; Group remaining buffers by directory, then major mode.
          (auto-directory)
          (auto-mode))))

(provide 'jp-bufler)
;;; jp-bufler.el ends here
