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
                   :models '(deepseek-r1:14b))))


;; (use-package ellama
;;   :straight t
;;   :init
;;   ;; setup key bindings
;;   ;; (setopt ellama-keymap-prefix "C-c e")
;;   ;; language you want ellama to translate to
;;   (setopt ellama-language "English")
;;   ;; could be llm-openai for example
;;   (require 'llm-ollama)
;;   (setopt ellama-provider
;;       (make-llm-ollama
;;        ;; this model should be pulled to use it
;;        ;; value should be the same as you print in terminal during pull
;;        :chat-model "deepseek-r1:14b"
;;        :embedding-model "nomic-embed-text"
;;        :default-chat-non-standard-params '(("num_ctx" . 8192))))
;;   (setopt ellama-summarization-provider
;;       (make-llm-ollama
;;        :chat-model "qwen2.5:3b"
;;        :embedding-model "nomic-embed-text"
;;        :default-chat-non-standard-params '(("num_ctx" . 32768))))
;;   (setopt ellama-coding-provider
;;       (make-llm-ollama
;;        :chat-model "qwen2.5-coder:3b"
;;        :embedding-model "nomic-embed-text"
;;        :default-chat-non-standard-params '(("num_ctx" . 32768))))
;;   ;; Naming new sessions with llm
;;   (setopt ellama-naming-provider
;;       (make-llm-ollama
;;        :chat-model "deepseek-r1:14b"
;;        :embedding-model "nomic-embed-text"
;;        :default-chat-non-standard-params '(("stop" . ("\n")))))
;;   (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
;;   ;; Translation llm provider
;;   (setopt ellama-translation-provider
;;       (make-llm-ollama
;;        :chat-model "qwen2.5:3b"
;;        :embedding-model "nomic-embed-text"
;;        :default-chat-non-standard-params
;;        '(("num_ctx" . 32768))))
;;   (setopt ellama-extraction-provider (make-llm-ollama
;;                 :chat-model "qwen2.5-coder:7b-instruct-q8_0"
;;                 :embedding-model "nomic-embed-text"
;;                 :default-chat-non-standard-params
;;                 '(("num_ctx" . 32768))))
;;   ;; customize display buffer behaviour
;;   ;; see ~(info "(elisp) Buffer Display Action Functions")~
;;   (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
;;   (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
;;   :config
;;   ;; send last message in chat buffer with C-c C-c
;;   (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message))


(provide 'jp-llm)
;;; jp-llm.el ends here
