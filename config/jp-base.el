(eval-when-compile
  (require 'use-package))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (progn
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
  :commands (xml-pretty-print rename-file-and-buffer macroexpand-point)
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

;; aggressive-indent
(use-package aggressive-indent
  :config
  (progn
    (global-aggressive-indent-mode 1)
    (add-to-list 'aggressive-indent-excluded-modes 'go-mode)))

;; which-key
(use-package which-key
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-add-key-based-replacements
    "M-m /" "search"
    "M-m g" "git"
    "M-m l" "layouts"
    "M-m p" "projects"
    "M-m t" "toggles"
    "M-m w" "workspace")
  (which-key-mode))

(provide 'jp-base)
