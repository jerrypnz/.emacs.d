(eval-when-compile
  (require 'use-package))

(use-package scala-mode)
(use-package sbt-mode)

(use-package ensime
  :commands (ensime))

(provide 'jp-scala)
