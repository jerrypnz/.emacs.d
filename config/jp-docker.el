(eval-when-compile
  (require 'use-package))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(provide 'jp-docker)
