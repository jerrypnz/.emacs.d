;;; jp-treemacs.el --- Treemacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'major-mode-hydra)

(use-package treemacs
  :straight t
  :config
  (progn
    (setq treemacs-follow-after-init t
          treemacs-is-never-other-window t
          treemacs-project-follow-cleanup t
          treemacs-tag-follow-delay 0.2
          treemacs-collapse-dirs 3
          treemacs-width 40)

    (treemacs-follow-mode t)
    (treemacs-tag-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-git-mode 'simple)
    (treemacs-fringe-indicator-mode nil)
    (treemacs-resize-icons 16)

    (treemacs-create-theme "all-the-icons"
      :config
      (progn
        (treemacs-create-icon
         :icon (concat " " (all-the-icons-octicon
                            "repo"
                            :height 1.1
                            :v-adjust 0
                            :face '(:inherit font-lock-doc-face :slant normal))
                       " ")
         :extensions (root))
        (treemacs-create-icon
         :icon (concat (all-the-icons-octicon
                        "chevron-right"
                        :height 0.75
                        :face '(:inherit font-lock-doc-face :slant normal))
                       " "
                       (all-the-icons-octicon
                        "file-directory"
                        :v-adjust 0
                        :face '(:inherit font-lock-doc-face :slant normal))
                       " ")
         :extensions (dir-closed))
        (treemacs-create-icon
         :icon (concat (all-the-icons-octicon
                        "chevron-down"
                        :height 0.75
                        :face '(:inherit font-lock-doc-face :slant normal))
                       " "
                       (all-the-icons-octicon
                        "file-directory"
                        :v-adjust 0
                        :face '(:inherit font-lock-doc-face :slant normal))
                       " ")
         :extensions (dir-open))
        (treemacs-create-icon
         :icon (concat "  " (all-the-icons-octicon "file-text" :v-adjust 0) " ")
         :extensions (fallback))
        (treemacs-create-icon
         :icon (concat "  " (all-the-icons-octicon "file-binary" :v-adjust 0) " ")
         :extensions ("exe" "so" "class" "elc" "doc" "docx" "xls" "xlsx" "pst"))
        (treemacs-create-icon
         :icon (concat "  " (all-the-icons-octicon "file-zip" :v-adjust 0) " ")
         :extensions ("tar" "zip" "gz" "bz2" "xz" "7z" "jar"))
        (treemacs-create-icon
         :icon (concat "  " (all-the-icons-octicon "file-media" :v-adjust 0) " ")
         :extensions ("jpg" "jpeg" "png" "svg" "bmp"))
        (treemacs-create-icon
         :icon (concat "  " (all-the-icons-octicon "file-pdf" :v-adjust 0) " ")
         :extensions ("pdf"))
        (treemacs-create-icon
         :icon (concat "  " (all-the-icons-octicon "file-code" :v-adjust 0) " ")
         :extensions ("yaml" "yml" "json" "xml" "html" "htm" "toml" "ini" "clj" "cljs" "cljc" "edn" "js" "c" "cpp" "c++"
                      "lisp" "el" "tpl" "proto" "java" "scala" "go" "lua" "conf" ))))

    (treemacs-load-theme "all-the-icons")

    (major-mode-hydra-bind treemacs-mode "Basic"
      ("?" treemacs-helpful-hydra/body "helpful"))
    (major-mode-hydra-bind treemacs-mode "Workspace"
      ("wc" treemacs-create-workspace "create workspace")
      ("wo" treemacs-switch-workspace "select workspace")
      ("wD" treemacs-remove-workspace "remove workspace"))
    (major-mode-hydra-bind treemacs-mode"Project"
      ("pp" treemacs-projectile "add project")
      ("pd" treemacs-remove-project-from-workspace "remove project")
      ("pr" treemacs-rename-project "rename project")
      ("pc" treemacs-collapse-project "collapse project")
      ("pC" treemacs-collapse-all-projects "collapse all projects"))))

(use-package treemacs-projectile
  :straight t
  :after (:and treemacs projectile))

(use-package treemacs-magit
  :straight t
  :after (:and treemacs magit))

(provide 'jp-treemacs)
;;; jp-treemacs.el ends here
