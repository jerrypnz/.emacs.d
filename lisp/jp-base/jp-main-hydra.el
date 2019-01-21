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
(require 'jp-commands)

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
(autoload 'org-capture "org-capture")
(autoload 'org-agenda "org-agenda")

(defvar jp-main-hydra--title
  (s-concat "\n "
            (s-repeat 60 " ")
            (all-the-icons-faicon "keyboard-o" :v-adjust 0.01 :height 1.1)
            (propertize " Main Hydra\n" 'face '(:height 1.1 :weight bold))))

(defun with-faicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0.05) :height (or height 0.7)) " " str))

(defun with-fileicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0.05) :height (or height 0.7)) " " str))

(pretty-hydra-define jp-main-hydra
  (:hint nil :color teal :quit-key "q" :title jp-main-hydra--title)
  ("Files & Buffers"
   (("f" jp-open-file "open file")
    ("b" jp-switch-buffer "switch buffers")
    ("/" jp-search "search")
    ("*" jp-search-symbol-at-pt "symbol at pt")
    ("TAB" jp-switch-to-previous-buffer "prev buffer"))

   "Tools"
   (("r" ivy-resume "ivy resume")
    ("m" major-mode-hydra "major mode hydra")
    ("w" jp-window/body "window management")
    ("R" jp-rectangle/body "rectangle")
    ("F" jp-flycheck/body "flycheck")
    ("l" jp-layouts/body "layouts")
    ("M" hydra-macro/body "keyboard macros")
    ("SPC" counsel-M-x "M-x")
    ;; The hydra is bound to M-SPC, pressing it again closes it.
    ("M-SPC" nil nil))

   "Projects"
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

   "Windows & Layouts"
   (("1" eyebrowse-switch-to-window-config-1 (jp-eyebrowse-layout-tag 1))
    ("2" eyebrowse-switch-to-window-config-2 (jp-eyebrowse-layout-tag 2))
    ("3" eyebrowse-switch-to-window-config-3 (jp-eyebrowse-layout-tag 3))
    ("4" eyebrowse-switch-to-window-config-4 (jp-eyebrowse-layout-tag 4))
    ("5" eyebrowse-switch-to-window-config-5 (jp-eyebrowse-layout-tag 5))
    ("6" eyebrowse-switch-to-window-config-6 (jp-eyebrowse-layout-tag 6))
    ("7" eyebrowse-switch-to-window-config-7 (jp-eyebrowse-layout-tag 7))
    ("8" eyebrowse-switch-to-window-config-8 (jp-eyebrowse-layout-tag 8))
    ("9" eyebrowse-switch-to-window-config-9 (jp-eyebrowse-layout-tag 9))
    ("l" jp-layouts/body "window layouts")
    ("w" jp-window/body "window management"))))

(provide 'jp-main-hydra)
