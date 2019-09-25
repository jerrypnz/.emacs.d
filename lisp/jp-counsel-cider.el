;;; jp-counsel-cider.el --- Search clojure vars with auto completion -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'dash)
(require 's)

(require 'ivy)
(require 'cider-connection)
(require 'cider-find)
(require 'cider-client)
(require 'cider-doc)
(require 'cider-repl-history)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counsel-cider-apropos

(defun jp-counsel-cider--apropos-function (input)
  (if (not (cider-current-repl))
      (list "" "No linked CIDER session")
    (or (ivy-more-chars)
        (-if-let (results (ignore-errors (cider-sync-request:apropos input nil nil nil t)))
            (-map (lambda (result)
                    (nrepl-dbind-response result (name type doc)
                      (s-concat name " " type " " doc)))
                  results)
          (list "" "Something goes wrong with the CIDER request")))))

(defun jp-counsel-cider--get-symbol (candidate)
  (car (s-split-up-to " " candidate 2)))

(defun jp-counsel-cider-apropos--action-find (candidate)
  (cider-find-var nil (jp-counsel-cider--get-symbol candidate)))

(defun jp-counsel-cider-apropos--action-doc (candidate)
  (cider-doc-lookup (jp-counsel-cider--get-symbol candidate)))

(defvar jp-counsel-cider-apropos--actions
  '(1
    ("o" jp-counsel-cider-apropos--action-find "find")
    ("d" jp-counsel-cider-apropos--action-doc "doc")))

(defun jp-counsel-cider-apropos--transformer (str)
  (if (not (s-contains? "/" str))
      str
    (-let* (((name type doc) (s-split-up-to " " str 2))
            ((ns var)        (s-split-up-to "/" name 1))
            ((label face)    (cond
                              ((string= type "function")     '("[f]" font-lock-function-name-face))
                              ((string= type "macro")        '("[m]" font-lock-keyword-face))
                              ((string= type "variable")     '("[v]" font-lock-variable-name-face))
                              ((string= type "special-form") '("[s]" font-lock-keyword-face))
                              (t                             '("[v]" font-lock-variable-name-face)))))
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
            :sort t
            :history 'jp-counsel-cider-apropos
            :caller 'jp-counsel-cider-apropos))

(ivy-set-display-transformer 'jp-counsel-cider-apropos
                             #'jp-counsel-cider-apropos--transformer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counsel-cider-repl-history

;; Defined in cider-repl-history.el
(defvar cider-repl-history-repl-buffer)
(defvar cider-repl-history-repl-window)

(defun jp-counsel-cider-repl-history--transformer (str)
  (with-temp-buffer
    (clojure-mode-variables)
    (clojure-font-lock-setup)
    (insert str)
    (font-lock-ensure)
    (buffer-substring (point-min) (point-max))))

(defun jp-counsel-cider-repl-history--action (candidate)
  (with-selected-window cider-repl-history-repl-window
    (with-current-buffer cider-repl-history-repl-buffer
      (goto-char (point-max))
      (cider-repl-history-insert-and-highlight candidate))))

(defun jp-counsel-cider-repl-history ()
  (interactive)
  (let* ((repl-win (selected-window))
         (repl-buf (window-buffer repl-win)))
    (setq cider-repl-history-repl-buffer repl-buf)
    (setq cider-repl-history-repl-window repl-win)
    (let ((history (mapcar #'copy-sequence (cider-repl-history-get-history))))
      (ivy-read "Find: "
                history
                :require-match t
                :re-builder #'ivy--regex
                :action #'jp-counsel-cider-repl-history--action
                :history 'jp-counsel-cider-repl-history
                :caller 'jp-counsel-cider-repl-history))))

(ivy-set-display-transformer 'jp-counsel-cider-repl-history
                             #'jp-counsel-cider-repl-history--transformer)

(provide 'jp-counsel-cider)
;;; jp-counsel-cider.el ends here
