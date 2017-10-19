(eval-when-compile
  (require 'use-package))

;; magit
(use-package magit
  :defer 2
  :bind
  (("M-m g s" . magit-status)
   ("M-m g b" . magit-blame)
   ("M-m g l" . magit-log-current)
   ("M-m g L" . magit-log-buffer-file))

  :init
  ;; Since we're using git subtree without building it with Make, we
  ;; don't have "magit-version.el" file.  Get around it by setting the
  ;; version here explicitly.
  (setq magit-version "2.11.0"))

(provide 'jp-magit)
