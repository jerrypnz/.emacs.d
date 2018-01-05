;;; jp-commands.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(defun kill-default-buffer ()
  "Kill the currently active buffer -- set to C-x k so that users are not asked which buffer they want to kill."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(defun kill-buffer-if-file (buf)
  "Kill a buffer only if it is file-based."
  (when (buffer-file-name buf)
    (when (buffer-modified-p buf)
      (when (y-or-n-p (format "Buffer %s is modified - save it?" (buffer-name buf)))
        (save-some-buffers nil buf)))
    (set-buffer-modified-p nil)
    (kill-buffer buf)))

(defun kill-all-buffers ()
  "Kill all file-based buffers."
  (interactive)
  (mapc (lambda (buf) (kill-buffer-if-file buf))
        (buffer-list)))

(defun kill-buffer-and-window ()
  "Close the current window and kill the buffer it's visiting."
  (interactive)
  (progn
    (kill-buffer)
    (delete-window)))

(defun create-new-buffer ()
  "Create a new buffer named *new*[num]."
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*new*")))

(defun insert-semicolon-at-end-of-line ()
  "Add a closing semicolon from anywhere in the line."
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(defun comment-current-line-dwim ()
  "Comment or uncomment the current line."
  (interactive)
  (save-excursion
    (push-mark (beginning-of-line) t t)
    (end-of-line)
    (comment-dwim nil)))

(defun jp-newline-below ()
  (end-of-line)
  (newline-and-indent))

(defun jp-newline-above ()
  (if (eq (forward-line -1) 0)
      (jp-newline-below)
    (progn
      (beginning-of-line)
      (newline)
      (forward-line -1))))

(defun jp-newline (above-p)
  (interactive "P")
  (if above-p
      (jp-newline-above)
    (jp-newline-below)))

(defun join-next-line ()
  (interactive)
  (join-line t))

(defun xml-pretty-print (use-xmllint-p)
  (interactive "P")
  (save-excursion
    (shell-command-on-region
     (mark)
     (point)
     (if use-xmllint-p
         "xmllint --format -"
       "tidy -utf8 -xml -i -w 76 --indent-attributes true 2>/dev/null")
     (buffer-name) t)))

;;; line numbering
(defun toggle-linum ()
  (interactive)
  (if (bound-and-true-p linum-mode)
      (linum-mode -1)
    (linum-mode t)))

(setq linum-format " %4d ")

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; TODO Consider creating jp-elisp-utils and move this command there
;; souce: https://stackoverflow.com/questions/5925485/emacs-lisp-macro-stepper
(defun macroexpand-point (sexp)
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*"
    (emacs-lisp-mode)
    (define-key
      (current-local-map)
      (kbd "q")
      (lambda ()
        (interactive)
        (kill-buffer)))))

(provide 'jp-commonds)

;;; jp-commands.el ends here
