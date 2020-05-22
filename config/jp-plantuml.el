;;; jp-plantuml.el --- plantuml-mode and org integrations -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(defvar homebrew-plantuml-jar-path
  (car (last (file-expand-wildcards "/usr/local/Cellar/plantuml/*/libexec/plantuml.jar"))))

(use-package plantuml-mode
  :straight t
  :custom
  (plantuml-default-exec-mode 'jar)
  :config
  (progn
    (if homebrew-plantuml-jar-path
        (setq plantuml-jar-path homebrew-plantuml-jar-path)
      (message "PlantUML jar not found. Preview will not work."))

    (eval-after-load "org-src"
      '(add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

    (eval-after-load "ob-plantuml"
      '(when homebrew-plantuml-jar-path
        (setq org-plantuml-jar-path homebrew-plantuml-jar-path)))))

(use-package flycheck-plantuml
  :straight t
  :after (flycheck)
  :config
  (flycheck-plantuml-setup))

(provide 'jp-plantuml)
;;; jp-plantuml.el ends here
