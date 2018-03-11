;;; jp-layouts.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'jp-fancy-hydra)
(require 'eyebrowse)

(defun jp-eyebrowse-layout-tag (slot)
  (let* ((window-configs (eyebrowse--get 'window-configs))
         (window-config (assoc slot window-configs))
         (tag (nth 2 window-config))
         (name (cond
                ((not tag)          "") ; show nothing if it's not created yet
                ((= (length tag) 0) "default") ; default name if there is no tag
                (t                  tag)))
         (face (if (= slot (eyebrowse--get 'current-slot))
                   '(underline t)
                 'shadow)))
    (propertize name 'face face)))

(defancyhydra jp-layouts (:hint nil :foreign-keys warn)
  ("Actions"  (("n" eyebrowse-next-window-config   "next")
               ("p" eyebrowse-prev-window-config   "previous")
               ("o" eyebrowse-last-window-config   "last")
               ("r" eyebrowse-rename-window-config "rename")
               ("x" eyebrowse-close-window-config  "close")
               ("q" nil "quit"))
   "Layouts"  (("a" eyebrowse-switch-to-window-config-1 (jp-eyebrowse-layout-tag 1) :exit t)
               ("s" eyebrowse-switch-to-window-config-2 (jp-eyebrowse-layout-tag 2) :exit t)
               ("d" eyebrowse-switch-to-window-config-3 (jp-eyebrowse-layout-tag 3) :exit t)
               ("f" eyebrowse-switch-to-window-config-4 (jp-eyebrowse-layout-tag 4) :exit t)
               ("j" eyebrowse-switch-to-window-config-5 (jp-eyebrowse-layout-tag 5) :exit t)
               ("k" eyebrowse-switch-to-window-config-6 (jp-eyebrowse-layout-tag 6) :exit t)
               ("l" eyebrowse-switch-to-window-config-7 (jp-eyebrowse-layout-tag 7) :exit t))))

(provide 'jp-layouts)
;;; jp-layouts.el ends here
