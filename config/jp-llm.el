;;; jp-llm.el --- Configuration related to UI: font, theme, icons etc -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package gptel
  :straight t
  :config
  (setq
   gptel-model 'deepseek-r1:14b
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '(deepseek-r1:14b
                             deepseek-r1:32b
                             qwen2.5-coder:32b))))

(provide 'jp-llm)
;;; jp-llm.el ends here
