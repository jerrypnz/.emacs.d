;;; jp-defaults.el --- Some default settings.  -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package))

;; graphene starter kit. will be replaced one day
(use-package graphene-meta-theme)
(use-package graphene)

;; themes
(use-package monokai-theme)

(add-hook 'after-init-hook
 '(lambda ()
    (load-theme 'monokai t)))

(provide 'jp-defaults)
