(eval-when-compile
  (require 'use-package))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

(use-package graphene-meta-theme)
(use-package jp-look
  :config
  (progn
    (setq jp-default-font "Source Code Pro-12")
    (setq jp-variable-pitch-font "Lucida Grande-12")
    (setq jp-fixed-pitch-font "Source Code Pro-12")))

;; themes
(use-package monokai-theme
  :config
  (progn
    (load-theme 'monokai t)))

(use-package jp-env)
(use-package jp-programming)
(use-package jp-modeline)

;; some useful global commands
(use-package jp-commands
  :commands (xml-pretty-print rename-file-and-buffer)
  :bind (("C-x k"   . kill-default-buffer)
         ("C-x C-k" . kill-buffer-and-window)
         ("C-c n"   . create-new-buffer)
         ("C-;"     . insert-semicolon-at-end-of-line)
         ("C-M-;"   . comment-current-line-dwim)
         ("C-o"     . start-newline-after)
         ("M-o"     . start-newline-before)
         ("M-j"     . join-next-line)
         ("M-m t n" . toggle-linum)))

;; expand-region
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C--" . er/contract-region)))

;; smex
(use-package smex
  :defer t
  :init (setq-default smex-history-length 32))

(provide 'jp-base)
