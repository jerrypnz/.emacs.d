;;; jp-flycheck-hydra.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'hydra)

(autoload 'flycheck-list-errors "flycheck")
(autoload 'flycheck-error-list-set-filter "flycheck")
(autoload 'flycheck-next-error "flycheck")
(autoload 'flycheck-previous-error "flycheck")
(autoload 'flycheck-first-error "flycheck")

;; Taken from https://github.com/abo-abo/hydra/wiki/Flycheck
(defhydra jp-flycheck
  (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
        :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
        :hint nil)
  "Errors"
  ("f"  flycheck-error-list-set-filter                            "filter")
  ("n"  flycheck-next-error                                       "next")
  ("p"  flycheck-previous-error                                   "previous")
  ("<"  flycheck-first-error                                      "first")
  (">"  (progn (goto-char (point-max)) (flycheck-previous-error)) "last")
  ("q"  nil))

(provide 'jp-flycheck-hydra)
;;; jp-flycheck-hydra.el ends here
