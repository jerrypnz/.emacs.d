(eval-when-compile
  (require 'use-package))

;; gocode emacs-company package is shipped with gocode itself. try to find it in $GOPATH
;; and add it to the load path.
(defun jp-init-gocode-emacs-path ()
  (let* ((go-path (getenv "GOPATH"))
         (gocode-company-path (concat go-path "/src/github.com/nsf/gocode/emacs-company")))
    (if (file-directory-p gocode-company-path)
        (add-to-list 'load-path gocode-company-path)
      nil)))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :bind (:map go-mode-map
              ("M-." . godef-jump)))

(if (jp-init-gocode-emacs-path)
    (use-package company-go
      :config
      (add-hook 'go-mode-hook
                (lambda ()
                  (set (make-local-variable 'company-backends) '(company-go))
                  (company-mode))))
  (warn "gocode emacs-company package not found in $GOPATH, install gocode first with 'go get -u github.com/nsf/gocode'"))

(use-package jp-go-play
  :commands (go-play))

(provide 'jp-go)
