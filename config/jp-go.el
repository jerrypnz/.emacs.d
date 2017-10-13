(eval-when-compile
  (require 'use-package))

;; gocode emacs-company package is shipped with gocode itself. try to find it in $GOPATH
;; and add it to the load path.
(defun jp-init-gocode-emacs-path ()
  (let* ((go-path (getenv "GOPATH"))
         (gocode-company-path (concat go-path "/src/github.com/nsf/gocode/emacs-company")))
    (when (file-directory-p gocode-company-path)
      (add-to-list 'load-path gocode-company-path))))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :bind (:map go-mode-map
              ("M-." . godef-jump)))

(use-package company-go
  :if (jp-init-gocode-emacs-path)
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode))))

(use-package jp-go-play
  :commands (go-play))

(provide 'jp-go)
