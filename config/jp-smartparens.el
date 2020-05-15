;;; jp-smartparens.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

;; smartparens
(use-package smartparens
  :straight t
  :bind
  (:map smartparens-mode-map
   ("M-<right>" . sp-forward-slurp-sexp)
   ("M-<left>" . sp-forward-barf-sexp)
   ("M-S-<right>" . sp-backward-slurp-sexp)
   ("M-S-<left>" . sp-backward-slurp-sexp)
   ("M-<up>" . sp-raise-sexp)
   ("C-k" . sp-kill-hybrid-sexp)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   :map prog-mode-map
   ("C-M-q" . sp-indent-defun))

  :hook
  ((prog-mode
    css-mode
    sgml-mode
    html-mode
    inferior-emacs-lisp-mode
    lisp-interaction-mode) . smartparens-mode)

  :hook
  ((prog-mode
    css-mode
    sgml-mode
    html-mode
    inferior-emacs-lisp-mode
    lisp-interaction-mode) . show-smartparens-mode)

  :config
  (progn
    (require 'smartparens-config)

    (defun jp--sp-pair-on-newline (_id _action _context)
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

    (setq blink-matching-paren nil)
    (setq sp-highlight-pair-overlay nil
          sp-show-pair-delay 0)))

;; rainbow delimiters
(use-package rainbow-delimiters
  :straight t
  :hook
  ((prog-mode
    css-mode
    sgml-mode
    html-mode
    inferior-emacs-lisp-mode
    lisp-interaction-mode) . rainbow-delimiters-mode))

(provide 'jp-smartparens)
;;; jp-smartparens.el ends here
