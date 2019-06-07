;;; jp-elisp.el --- Clojure configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'major-mode-hydra))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp programming settings (found here: http://www.sugarshark.com/elisp/init/lisp.el.html)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.
This checks in turn:

-- for a function name where point is
-- for a variable name where point is
-- for a surrounding function call"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
                           (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (describe-function sym))
          ((setq sym (variable-at-point)) (describe-variable sym))
          ;; now let it operate fully -- i.e. also check the
          ;; surrounding sexp for a function call.
          ((setq sym (function-at-point)) (describe-function sym)))))

(major-mode-hydra-bind emacs-lisp-mode "Eval"
  ("b" eval-buffer "buffer")
  ("e" eval-defun "defun")
  ("r" eval-region "region"))

(major-mode-hydra-bind emacs-lisp-mode "Test"
  ("t" ert "prompt")
  ("T" (ert t) "all")
  ("F" (ert :failed) "failed"))

(major-mode-hydra-bind emacs-lisp-mode "Doc"
  ("d" describe-foo-at-point "thing-at-pt")
  ("f" describe-function)
  ("v" describe-variable))

;; Prefer xref-find than dump-jump for elisp
(define-key emacs-lisp-mode-map (kbd "M-.") #'xref-find-definitions)
(define-key emacs-lisp-mode-map (kbd "C-M-.") #'xref-find-apropos)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local lisp-indent-function
                        #'common-lisp-indent-function)))

(provide 'jp-elisp)
;;; jp-elisp.el ends here
