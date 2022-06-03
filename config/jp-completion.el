;;; jp-completion.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package wgrep
  :straight t)

(use-package vertico
  :straight t
  :init
  (vertico-mode)

  ;; Different scroll margin
  (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)
  )

(use-package consult
  :straight t
  :pretty-hydra
  (helpful-hydra
   (""
    (("a" consult-apropos "apropos"))))

  :bind
  (("M-y"     . consult-yank-pop)
   ("C-s"     . consult-line)))

(use-package jp-completion-utils
  :bind (("C-M-s" . jp-consult-line-symbol-at-point)))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))


;; Optionally use the `orderless' completion style.
(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :straight t

  :pretty-hydra
  (helpful-hydra
   (""
    (("B" embark-bindings "bindings"))))

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   )

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;(use-package jp-ivy-utils
;;  :bind
;;  ("C-M-s" . jp-swiper-symbol-at-pt))

;; company
(use-package company
  :straight t
  :config
  (progn
    (setq company-idle-delay 0.125
          company-minimum-prefix-length 1
          company-require-match nil
          company-transformers '(company-sort-by-occurrence)
          company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                              company-preview-frontend
                              company-echo-metadata-frontend))
    (global-company-mode t)))

(use-package company-box
  :straight t
  :defer t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-enable-icon nil)
  ;; Remove once https://github.com/sebastiencs/company-box/pull/91 is merged
  (defun jp-company-box--render (f &rest args)
    (let ((x (apply f args)))
      (with-current-buffer (company-box--get-buffer)
        (setq header-line-format nil))))
  (advice-add #'company-box--render-buffer :around #'jp-company-box--render))

(use-package company-dabbrev
  :after (company)
  :config
  (progn
    (setq company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil)))

(use-package company-yasnippet
  :after (company)
  :bind
  ("C-M-y" . company-yasnippet))

(provide 'jp-completion)
;;; jp-completion.el ends here
