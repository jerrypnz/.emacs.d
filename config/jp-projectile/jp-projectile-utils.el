(autoload 'counsel-rg "counsel")
(autoload 'counsel-projectile-switch-to-buffer "counsel-projectile")
(autoload 'projectile-project-p "projectile")

(defun jp-switch-buffer ()
  (interactive)
  (if (projectile-project-p)
      (counsel-projectile-switch-to-buffer)
    (ivy-switch-buffer)))

(defun jp-search ()
  (interactive)
  (if (projectile-project-p)
      (counsel-projectile-rg)
    (counsel-rg)))

(provide 'jp-projectile-utils)
