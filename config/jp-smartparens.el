(eval-when-compile
  (require 'use-package))

;; smartparens
(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    
    (defun jp--sp-pair-on-newline (id action context)
      "Put trailing pair on newline and return to point."
      (save-excursion
        (newline)
        (indent-according-to-mode)))

    (defun jp--sp-pair-on-newline-and-indent (id action context)
      "Open a new brace or bracket expression, with relevant newlines and indent. "
      (jp--sp-pair-on-newline id action context)
      (indent-according-to-mode))

    (sp-pair "{" nil :post-handlers
             '(:add ((lambda (id action context)
                       (jp--sp-pair-on-newline-and-indent id action context)) "RET")))
    (sp-pair "[" nil :post-handlers
             '(:add ((lambda (id action context)
                       (jp--sp-pair-on-newline-and-indent id action context)) "RET")))

    (sp-local-pair '(markdown-mode gfm-mode) "*" "*"
                   :unless '(sp-in-string-p)
                   :actions '(insert wrap))

    (setq sp-highlight-pair-overlay nil)
    
    (smartparens-global-mode t)))

(provide 'jp-smartparens)
