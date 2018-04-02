;;; jp-restclient.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package restclient
  :init
  (progn
    (major-mode-hydra-bind restclient-mode "Navigate"
      ("n" restclient-jump-next "next" :exit nil)
      ("p" restclient-jump-prev "previous" :exit nil)
      ("N" restclient-narrow-to-current "narrow")
      ("W" widen "widen")
      ("q" nil "quit"))
    (major-mode-hydra-bind restclient-mode "Send"
      ("s" restclient-http-send-current-stay-in-window "send" :exit nil)
      ("S" restclient-http-send-current "send and jump")
      ("r" restclient-http-send-current-raw "send raw"))
    (major-mode-hydra-bind restclient-mode "Misc"
      ("w" restclient-copy-curl-command "copy curl")
      ("m" restclient-mark-current "mark")
      ("q" nil "quit")))
  :commands (restclient-mode))

(provide 'jp-restclient)
;;; jp-restclient.el ends here
