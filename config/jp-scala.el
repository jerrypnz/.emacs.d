;;; jp-scala.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package scala-mode
  :straight t
  :mode ("\\.scala\\'" "\\.sbt\\'")
  :defer t
  :init
  (progn
    (dolist (ext '(".cfe" ".cfs" ".si" ".gen" ".lock"))
      (add-to-list 'completion-ignored-extensions ext)))
  :config
  (progn
    (require 'jp-scala-editing)
    (add-hook 'scala-mode-hook
              (lambda ()
                ;; Add the hook to the end of the list and use buffer local hooks
                ;; because we want to enable this hook only in scala-mode.
                (add-hook 'post-command-hook 'jp-scala-insert-margin-befor-ending-parens t t)))

    (define-key scala-mode-map (kbd "RET") 'jp-scala-newline-and-indent-with-pipe)
    (define-key scala-mode-map (kbd ">") 'jp-scala-gt)
    (define-key scala-mode-map (kbd "-") 'jp-scala-hyphen)
    (define-key scala-mode-map (kbd "M-j") 'jp-scala-join-line)

    ;; Compatibility with `aggressive-indent'
    (setq scala-indent:align-forms t
          scala-indent:align-parameters t
          scala-indent:default-run-on-strategy
          scala-indent:operator-strategy)))

(use-package sbt-mode
  :straight t)

(use-package ensime
  ;; Have to do this to get around https://github.com/raxod502/straight.el/issues/279
  :straight (:host github :repo "ensime/ensime-emacs" :branch "2.0")
  :commands (ensime)
  :config
  (progn
    ;; Disable startup notification
    (setq ensime-startup-notification nil)
    ;; Enable Expand Region integration from Ensime.  Ignore load errors to
    ;; handle older Ensime versions gracefully.
    (require 'ensime-expand-region nil 'noerror)))

(provide 'jp-scala)
;;; jp-scala.el ends here
