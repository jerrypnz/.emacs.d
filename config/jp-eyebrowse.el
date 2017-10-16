(eval-when-compile
  (require 'use-package))

(use-package eyebrowse
  :config
  (progn
    ;; Switch to a project and use its name (shorten) as the tag of
    ;; the window config TODO `shorten-directory' is defined in
    ;; `jp-modeline.el' - move it to a common lib
    (defun jp-eyebrowse-new-project-workspace ()
      (interactive)
      (let ((project-name (counsel-projectile-switch-project)))
        (when (> (length project-name) 0)
          (eyebrowse-rename-window-config (eyebrowse--get 'current-slot)
                                          (shorten-directory project-name 32)))))

    (setq eyebrowse-new-workspace 'jp-eyebrowse-new-project-workspace)
    (setq eyebrowse-tagged-slot-format "%s [%t]")
    (eyebrowse-mode t)))

(use-package jp-window-layouts
  :bind
  ("M-m l" . jp-window-layouts/body))

(provide 'jp-eyebrowse)
