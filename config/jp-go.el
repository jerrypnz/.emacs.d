(eval-when-compile
  (require 'use-package))

;; gocode emacs-company package is shipped with gocode itself. try to find it in $GOPATH
;; and add it to the load path.
(defun jp-init-gocode-emacs-path ()
  (let* ((go-path (getenv "GOPATH"))
         (gocode-company-path (concat go-path "/src/github.com/nsf/gocode/emacs-company")))
    (if (file-directory-p gocode-company-path)
        (add-to-list 'load-path gocode-company-path)
      (message "gocode emacs-company not found in $GOPATH"))))

(jp-init-gocode-emacs-path)

(use-package go-mode
  :bind
  (:map go-mode-map
        ("M-." . godef-jump)))

(use-package company-go
  :defer t)

(use-package jp-go-play
  :commands (go-play))

(provide 'jp-go)
