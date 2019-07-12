;;; jp-layouts.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'pretty-hydra)
(require 'eyebrowse)

(autoload 'counsel-projectile-switch-project "counsel-projectile")

;; Switch to a project and use its name as the tag of
;; the window config
(defun jp-eyebrowse-switch-project ()
  (interactive)
  (let ((project-name (counsel-projectile-switch-project)))
    (when (> (length project-name) 0)
      (eyebrowse-rename-window-config
       (eyebrowse--get 'current-slot)
       ;;(shorten-directory project-name 32)
       (file-name-nondirectory (directory-file-name project-name))))))

(defun jp-eyebrowse-layout-tag (slot)
  (let* ((window-configs (eyebrowse--get 'window-configs))
         (window-config (assoc slot window-configs))
         (tag (nth 2 window-config))
         (name (cond
                ((not tag)          "") ; show nothing if it's not created yet
                ((= (length tag) 0) "default") ; default name if there is no tag
                (t                  tag)))
         (face (if (= slot (eyebrowse--get 'current-slot)) 'underline 'shadow)))
    (propertize name 'face face)))

(defvar jp-layout--title)
(setq jp-layout--title (with-faicon "windows" "Window Layouts" 1 -0.05))

(pretty-hydra-define jp-layouts (:foreign-keys warn :quit-key "q" :title jp-layout--title)
  ("Actions"
   (("TAB" eyebrowse-last-window-config   "last")
    ("n" eyebrowse-next-window-config   "next")
    ("p" eyebrowse-prev-window-config   "previous")
    ("r" eyebrowse-rename-window-config "rename")
    ("x" eyebrowse-close-window-config  "close"))

   "Layouts"
   (("1" eyebrowse-switch-to-window-config-1 (jp-eyebrowse-layout-tag 1) :exit t :width 20)
    ("2" eyebrowse-switch-to-window-config-2 (jp-eyebrowse-layout-tag 2) :exit t)
    ("3" eyebrowse-switch-to-window-config-3 (jp-eyebrowse-layout-tag 3) :exit t)
    ("4" eyebrowse-switch-to-window-config-4 (jp-eyebrowse-layout-tag 4) :exit t)
    ("5" eyebrowse-switch-to-window-config-5 (jp-eyebrowse-layout-tag 5) :exit t)
    ("6" eyebrowse-switch-to-window-config-6 (jp-eyebrowse-layout-tag 6) :exit t)
    ("7" eyebrowse-switch-to-window-config-7 (jp-eyebrowse-layout-tag 7) :exit t)
    ("8" eyebrowse-switch-to-window-config-8 (jp-eyebrowse-layout-tag 8) :exit t)
    ("9" eyebrowse-switch-to-window-config-9 (jp-eyebrowse-layout-tag 9) :exit t))))

(provide 'jp-layouts)
;;; jp-layouts.el ends here
