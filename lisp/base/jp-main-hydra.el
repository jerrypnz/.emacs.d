;;; jp-main-hydra.el --- My main hydra -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'pretty-hydra)
(require 'major-mode-hydra)
(require 'jp-icons)
(require 'jp-rect)
(require 'jp-macro)
(require 'jp-layouts)
(require 'jp-projectile-utils)
(require 'jp-window)
(require 'jp-flycheck-hydra)
(require 'jp-commands)
(require 'jp-themes)

(autoload 'counsel-projectile "counsel-projectile")
(autoload 'counsel-projectile "counsel-projectile")
(autoload 'counsel-projectile-find-dir "counsel-projectile")
(autoload 'projectile-ibuffer "projectile")

(autoload 'magit-status "magit")
(autoload 'magit-blame "magit")
(autoload 'magit-log-current "magit")
(autoload 'magit-log-buffer-file "magit")
(autoload 'git-timemachine "git-timemachine")
(autoload 'treemacs-select-window "treemacs")

(autoload 'eyebrowse-switch-to-window-config-1 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-2 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-3 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-4 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-5 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-6 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-7 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-8 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-9 "eyebrowse")

(autoload 'org-capture "org-capture")
(autoload 'jp-org-agenda/body "jp-org-agenda")

(defvar jp-toggles--title)
(setq jp-toggles--title (with-faicon "toggle-on" "Toggles" 1 -0.05))

(pretty-hydra-define jp-toggles
  (:hint nil :color amaranth :quit-key "q" :title jp-toggles--title)
  ("Basic"
   (("n" linum-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("W" whitespace-cleanup-mode "whitespace cleanup" :toggle t)
    ("r" rainbow-mode "rainbow" :toggle t)
    ("L" page-break-lines-mode "page break lines" :toggle t))
   "Highlight"
   (("s" symbol-overlay-mode "symbol" :toggle t)
    ("l" hl-line-mode "line" :toggle t)
    ("x" highlight-sexp-mode "sexp" :toggle t)
    ("t" hl-todo-mode "todo" :toggle t))
   "UI"
   (("d" jp-themes-toggle-light-dark (pretty-hydra-toggle "dark theme" jp-current-theme-dark-p)))
   "Coding"
   (("p" smartparens-mode "smartparens" :toggle t)
    ("P" smartparens-strict-mode "smartparens strict" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t))
   "Emacs"
   (("D" toggle-debug-on-error (pretty-hydra-toggle "debug on error" (default-value 'debug-on-error)))
    ("X" toggle-debug-on-quit  (pretty-hydra-toggle "debug on quit" (default-value 'debug-on-quit))))))

(defun jp-projects--title ()
  (let ((p (projectile-project-name)))
    (with-octicon "repo"
                  (if (s-blank-p p)
                      "Projects"
                    (s-concat "Projects (current: " p ")")))))

(pretty-hydra-define jp-projects
  (:hint nil :color teal :quit-key "q" :title (jp-projects--title))
  ("Current Project"
   (("f" counsel-projectile "open file/buffer")
    ("b" counsel-projectile-switch-to-buffe "switch to buffer")
    ("d" counsel-projectile-find-dir "open directory")
    ("i" projectile-ibuffer "ibuffer")
    ("k" projectile-kill-buffers "kill buffers")
    ("I" projectile-invalidate-cache "invalidate cache"))
   "All Projects"
   (("p" jp-eyebrowse-switch-project "switch")
    ("r" jp-refresh-projectile-projects "refresh project list"))))

(defvar jp-git--title)
(setq jp-git--title (with-octicon "git-compare" "Git"))

(pretty-hydra-define jp-git
  (:hint nil :color teal :quit-key "q" :title jp-git--title)
  ("Actions"
   (("s" magit-status "magit status")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("b" magit-blame-addition "blame")
    ("t" git-timemachine "time machine"))))

(defvar jp-main-hydra--title)
(setq jp-main-hydra--title
      (with-faicon "keyboard-o" (propertize "Main Dispatcher\n" 'face '(:weight bold :height 1.1))
                   1.1 0.01))

(pretty-hydra-define jp-main-hydra
  (:hint nil :color teal :quit-key "q" :title jp-main-hydra--title)
  ("Actions"
   (("TAB" jp-switch-to-previous-buffer "prev buffer")
    ("SPC" counsel-M-x "M-x")
    ("f" jp-open-file "open file")
    ("b" jp-switch-buffer "switch buffers")
    ("r" ivy-resume "ivy resume")
    ("," treemacs-select-window "treemacs")
    ("/" jp-search "search")
    ("*" jp-search-symbol-at-pt "search symbol at pt")
    ("F" jp-flycheck/body "flycheck")
    ;; The hydra is bound to M-SPC, pressing it again closes it.
    ("M-SPC" nil nil))

   "Menus"
   (("p" jp-projects/body "projects...")
    ("g" jp-git/body "git...")
    ("m" major-mode-hydra "major mode...")
    ("R" jp-rectangle/body "rectangle...")
    ("M" hydra-macro/body "macros...")
    ("w" jp-window/body "windows...")
    ("l" jp-layouts/body "layouts...")
    ("t" jp-toggles/body "toggles..."))

   "Org"
   (("c" org-capture "capture")
    ("a" jp-org-agenda/body "agenda"))

   "Quick Layouts"
   (("1" eyebrowse-switch-to-window-config-1 (jp-eyebrowse-layout-tag 1) :width 20)
    ("2" eyebrowse-switch-to-window-config-2 (jp-eyebrowse-layout-tag 2))
    ("3" eyebrowse-switch-to-window-config-3 (jp-eyebrowse-layout-tag 3))
    ("4" eyebrowse-switch-to-window-config-4 (jp-eyebrowse-layout-tag 4))
    ("5" eyebrowse-switch-to-window-config-5 (jp-eyebrowse-layout-tag 5))
    ("6" eyebrowse-switch-to-window-config-6 (jp-eyebrowse-layout-tag 6))
    ("7" eyebrowse-switch-to-window-config-7 (jp-eyebrowse-layout-tag 7))
    ("8" eyebrowse-switch-to-window-config-8 (jp-eyebrowse-layout-tag 8))
    ("9" eyebrowse-switch-to-window-config-9 (jp-eyebrowse-layout-tag 9)))))

(provide 'jp-main-hydra)
