;;; jp-projectile.el --- Helper commands for projectile

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:


(autoload 'ivy-switch-buffer "ivy")
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
;;;jp-projectile-utils ends here
