(eval-when-compile
  (require 'use-package))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (progn
    (setq markdown-command "marked")
    (add-hook 'markdown-mode-hook #'turn-on-auto-fill)))

(provide 'jp-markdown)
