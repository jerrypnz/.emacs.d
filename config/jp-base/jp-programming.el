(defvar jp-prog-mode-hook nil
  "A hook to be run on entering a de facto prog mode.")

(defvar jp-prog-mode-hooks
  '(prog-mode-hook    
    css-mode-hook
    sgml-mode-hook
    html-mode-hook))

;; Main hook to be run on entering de facto prog modes
(add-hook 'jp-prog-mode-hook
          (lambda ()
            (jp-indent)
            (jp-linum)
            (jp-pairs)
            (jp-show-pairs)
            (jp-completion)
            (jp-errors)))

;; Attach de facto prog mode hooks after loading init file
(add-hook 'after-init-hook
          (lambda ()
            (dolist (hook jp-prog-mode-hooks)
              (add-hook hook (lambda () (run-hooks 'jp-prog-mode-hook))))))


;;; indenting

(defun jp-indent ()
  (electric-indent-mode t))

;;; line numbering
;; TODO Make it a toggle rather than always enabling it
(defun jp-linum ()
  (linum-mode t))

(setq linum-format " %4d ")

(autoload 'smartparens-strict-mode "smartparens")
(autoload 'show-smartparens-mode "smartparens")
(autoload 'company-mode "company")
(autoload 'flycheck-mode "flycheck")

;;; auto-pairing
(defun jp-pairs ()  
  (smartparens-strict-mode t))

(defun jp-show-pairs ()
  (show-paren-mode nil)
  (setq blink-matching-paren nil)  
  (show-smartparens-mode)
  (setq sp-show-pair-delay 0))

;;; completion
(defun jp-completion ()  
  (company-mode t))

;;; error checking
(defun jp-errors ()  
  (flycheck-mode))

;; Delete marked text on typing
(delete-selection-mode t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; Don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Better scrolling with mouse wheel/trackpad.
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))

;; Character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; apply syntax highlighting to all buffers
(global-font-lock-mode t)

;; auto json-mode
(push '("\\.json\\'" . json-mode) auto-mode-alist)

;; 2-space indent for CSS
(setq css-indent-offset 2)

(provide 'jp-programming)

