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
(autoload 'major-mode-hydras/org-agenda-mode/body "jp-org-agenda")
(autoload 'deadgrep "deadgrep")

(defvar jp-toggles--title)
(setq jp-toggles--title (with-faicon "toggle-on" "Toggles" 1 -0.05))

(pretty-hydra-define jp-toggles
  (:color amaranth :quit-key "q" :title jp-toggles--title)
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
   (("d" jp-themes-toggle-light-dark "dark theme" :toggle jp-current-theme-dark-p))
   "Coding"
   (("p" smartparens-mode "smartparens" :toggle t)
    ("P" smartparens-strict-mode "smartparens strict" :toggle t)
    ("S" show-smartparens-mode "show smartparens" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t)
    ("C" mac-auto-operator-composition-mode "ligature" :toggle t))
   "Emacs"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))

(defun jp-projects--title ()
  (let ((p (projectile-project-name)))
    (with-octicon "repo"
                  (if (s-blank-p p)
                      "Projects"
                    (s-concat "Projects (current: " p ")")))))

(pretty-hydra-define jp-projects
  (:color teal :quit-key "q" :title (jp-projects--title))
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

(defvar jp-main-hydra--title)
(setq jp-main-hydra--title
      (with-faicon "keyboard-o" (propertize "Main Dispatcher\n" 'face '(:weight bold :height 1.1))
                   1.1 0.01))

(pretty-hydra-define jp-main-hydra
  (:color teal :quit-key ("q" "M-SPC") :title jp-main-hydra--title)
  ("Actions"
   (("TAB" jp-switch-to-previous-buffer "prev buffer")
    ("SPC" counsel-M-x "M-x")
    ("f" jp-open-file "open file")
    ("b" jp-switch-buffer "switch buffers")
    ("r" ivy-resume "ivy resume")
    ("i" counsel-imenu "imenu")
    ("j" counsel-mark-ring "mark ring")
    ("d" dash-at-point-hydra/body "dash")
    ("," treemacs-select-window "treemacs")
    ("/" jp-search "search")
    ("*" jp-search-symbol-at-pt "search symbol at pt")
    ("F" jp-flycheck/body "flycheck"))

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
    ("a" major-mode-hydras/org-agenda-mode/body "agenda"))

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
