;;; jp-main-hydra.el --- My main hydra -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'pretty-hydra)
(require 'major-mode-hydra)
(require 'all-the-icons)
(require 'jp-rect)
(require 'jp-macro)
(require 'jp-layouts)
(require 'jp-projectile-utils)
(require 'jp-window)
(require 'jp-flycheck-hydra)

(autoload 'counsel-projectile "counsel-projectile")
(autoload 'counsel-projectile "counsel-projectile")
(autoload 'counsel-projectile-find-dir "counsel-projectile")
(autoload 'projectile-ibuffer "projectile")

(autoload 'magit-status "magit")
(autoload 'magit-blame "magit")
(autoload 'magit-log-current "magit")
(autoload 'magit-log-buffer-file "magit")
(autoload 'git-timemachine "git-timemachine")

(autoload 'eyebrowse-switch-to-window-config-1 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-2 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-3 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-4 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-5 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-6 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-7 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-8 "eyebrowse")
(autoload 'eyebrowse-switch-to-window-config-9 "eyebrowse")

(autoload 'deft "deft")
(autoload 'org-capture "org")
(autoload 'org-agenda "org")

(defvar jp-main-hydra--title (s-concat "\n " (all-the-icons-faicon "cogs" :v-adjust 0.05) " Main Hydra"))

(pretty-hydra-define jp-main-hydra
  (:hint nil :color teal :quit-key "q" :title jp-main-hydra--title)
  ("Basic"
   (("f" jp-open-file "open file")
    ("b" jp-switch-buffer "switch buffers")
    ("r" ivy-resume "ivy resume")
    ("m" major-mode-hydra "major mode hydra")
    ("w" jp-window/body "window management")
    ("R" jp-rectangle/body "rectangle")
    ("F" jp-flycheck/body "flycheck")
    ("l" jp-layouts/body "layouts")
    ("/" jp-search "search")
    ("*" jp-search-symbol-at-pt "symbol at pt")
    ("M" hydra-macro/body "keyboard macros")
    ;; The hydra is bound to M-SPC, pressing it again closes it.
    ("M-SPC" nil nil))

   "Project"
   (("pp" jp-eyebrowse-switch-project "switch project")
    ("pf" counsel-projectile "file/buffer")
    ("pd" counsel-projectile-find-dir "directory")
    ("pi" projectile-ibuffer "ibuffer")
    ("pI" projectile-invalidate-cache "invalidate cache"))

   "Git"
   (("gs" magit-status "status")
    ("gl" magit-log-buffer-file "file log")
    ("gL" magit-log-current "project log")
    ("gb" magit-blame "blame")
    ("gt" git-timemachine "time machine"))

   "Org"
   (("oc" org-capture "capture")
    ("oa" org-agenda "agenda")
    ("on" deft "deft"))

   "Toggles"
   (("tn" toggle-linum "line number")
    ("tw" whitespace-mode "whitespace"))

   "Quick Layouts"
   (("1" eyebrowse-switch-to-window-config-1 (jp-eyebrowse-layout-tag 1))
    ("2" eyebrowse-switch-to-window-config-2 (jp-eyebrowse-layout-tag 2))
    ("3" eyebrowse-switch-to-window-config-3 (jp-eyebrowse-layout-tag 3))
    ("4" eyebrowse-switch-to-window-config-4 (jp-eyebrowse-layout-tag 4))
    ("5" eyebrowse-switch-to-window-config-5 (jp-eyebrowse-layout-tag 5))
    ("6" eyebrowse-switch-to-window-config-6 (jp-eyebrowse-layout-tag 6))
    ("7" eyebrowse-switch-to-window-config-7 (jp-eyebrowse-layout-tag 7))
    ("8" eyebrowse-switch-to-window-config-8 (jp-eyebrowse-layout-tag 8))
    ("9" eyebrowse-switch-to-window-config-9 (jp-eyebrowse-layout-tag 9)))))

(provide 'jp-main-hydra)
