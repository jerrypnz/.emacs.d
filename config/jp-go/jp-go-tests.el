;;; jp-go-tests.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(defvar go-use-gocheck-for-testing nil)

(defun jp-go-run-tests (args)
  (interactive)
  (save-selected-window
    (compile (concat "go test " args))))

(defun jp-go-run-package-tests ()
  (interactive)
  (jp-go-run-tests ""))

(defun jp-go-run-package-tests-nested ()
  (interactive)
  (jp-go-run-tests "./..."))

(defun jp-go-run-test-current-function ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (let ((test-method (if go-use-gocheck-for-testing
                             "-check.f"
                           "-run")))
        (save-excursion
          (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
          (jp-go-run-tests (concat test-method "='" (match-string-no-properties 2) "'"))))
    (message "Must be in a _test.go file to run go-run-test-current-function")))

(defun jp-go-run-test-current-suite ()
  (interactive)
  (if (string-match "_test\.go" buffer-file-name)
      (if go-use-gocheck-for-testing
          (save-excursion
            (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?Test[[:alnum:]_]+(.*)")
            (jp-go-run-tests (concat "-check.f='" (match-string-no-properties 2) "'")))
        (message "Gocheck is needed to test the current suite"))
    (message "Must be in a _test.go file to run go-test-current-suite")))

(provide 'jp-go-tests)
;; jp-go-tests.el ends here
