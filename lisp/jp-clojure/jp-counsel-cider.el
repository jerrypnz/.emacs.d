;;; jp-counsel-cider.el --- Search clojure vars with auto completion -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'dash)
(require 's)

(autoload 'cider-current-repl "cider-connection")
(autoload 'cider--find-var "cider-find")
(autoload 'cider-sync-request:apropos "cider-client")
(autoload 'cider-doc-lookup "cider-doc")
(autoload 'cider-grimoire-lookup "cider-grimoire")

(defun jp-counsel-cider--apropos-function (input)
  (if (not (cider-current-repl))
      (list "" "No linked CIDER session")
    (or (counsel-more-chars)
        (-if-let (results (ignore-errors (cider-sync-request:apropos input nil nil nil t)))
            (-map (lambda (result)
                    (nrepl-dbind-response result (name type doc)
                      (s-concat name " " type " " doc)))
                  results)
          (list "" "Something goes wrong with the CIDER request")))))

(defun jp-counsel-cider--get-symbol (candidate)
  (car (s-split-up-to " " candidate 2)))

(defun jp-counsel-cider-apropos--action-find (candidate)
  (cider--find-var (jp-counsel-cider--get-symbol candidate)))

(defun jp-counsel-cider-apropos--action-doc (candidate)
  (cider-doc-lookup (jp-counsel-cider--get-symbol candidate)))

(defun jp-counsel-cider-apropos--action-grimoire (candidate)
  (cider-grimoire-lookup (jp-counsel-cider--get-symbol candidate)))

(defvar jp-counsel-cider-apropos--actions
  '(1
    ("o" jp-counsel-cider-apropos--action-find "find")
    ("d" jp-counsel-cider-apropos--action-doc "doc")
    ("g" jp-counsel-cider-apropos--action-grimoire "grimoire")))

(defun jp-counsel-cider-apropos--transformer (str)
  (if (not (s-contains? "/" str))
      str
    (-let* (((name type doc) (s-split-up-to " " str 2))
            ((ns var)        (s-split-up-to "/" name 1))
            ((label face)    (cond
                              ((string= type "function")     '("⨍" font-lock-function-name-face))
                              ((string= type "macro")        '("⨎" font-lock-keyword-face))
                              ((string= type "variable")     '("𝒙" font-lock-variable-name-face))
                              ((string= type "special-form") '("⨐" font-lock-keyword-face))
                              (t                             '("𝒙" font-lock-variable-name-face)))))
      (format "%s %-32s %s"
              (propertize label 'face face)
              (format "%s/%s"
                      (propertize ns 'face 'font-lock-type-face)
                      (propertize var 'face face))
              (if (string= doc "(not documented)")
                  ""
                (propertize doc 'face 'font-lock-comment-face))))))

(defun jp-counsel-cider-apropos ()
  (interactive)
  (ivy-read "Find: "
            #'jp-counsel-cider--apropos-function
            :dynamic-collection t
            :require-match t
            :re-builder #'ivy--regex
            :action jp-counsel-cider-apropos--actions
            :history 'jp-counsel-cider-apropos
            :caller 'jp-counsel-cider-apropos))

(ivy-set-display-transformer 'jp-counsel-cider-apropos
                             #'jp-counsel-cider-apropos--transformer)

(provide 'jp-counsel-cider)
;;; jp-counsel-cider.el ends here
