;;; jp-modeline.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:
;;
;; Taken from http://amitp.blogspot.co.nz/2011/08/emacs-custom-mode-line.html with
;; some modifications
;;
;;; Code:

(require 'all-the-icons)
(require 's)

(defvar jp-modeline-active-window nil)

(defun jp-modeline-set-active-window (windows)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq jp-modeline-active-window (selected-window))))

(add-function :before pre-redisplay-function #'jp-modeline-set-active-window)

(defun jp-modeline-active-p ()
  (eq jp-modeline-active-window (get-buffer-window)))

(defun jp-modeline-status ()
  (let* ((active (jp-modeline-active-p))
         (face (if buffer-read-only
                   (if active 'mode-line-read-only-face 'mode-line-read-only-inactive-face)
                 (if active 'mode-line-read-write-face 'mode-line-read-write-inactive-face)))
         (icon (cond (buffer-read-only    "lock")
                     ((buffer-modified-p) "plus-circle")
                     (t                   "check-circle"))))
    (s-concat (propertize " " 'face face)
              (all-the-icons-faicon icon :height 0.8 :v-adjust 0.05 :face face)
              (propertize " " 'face face))))

(defun jp-buffer-filename ()
  (let* ((bufname (buffer-name))
         (name (file-name-nondirectory bufname)))
    (if (eq "" name)
        bufname
      name)))

;; Extra mode line faces
(defface mode-line-read-only-face
  '((t (:inherit 'mode-line)))
  "Face for read only indicator in mode line"
  :group 'jp-modeline)

(defface mode-line-read-only-inactive-face
  '((t (:inherit 'mode-line-inactive)))
  "Face for read only indicator in inactive mode line"
  :group 'jp-modeline)

(defface mode-line-read-write-face
  '((t (:inherit 'mode-line)))
  "Face for read write indicator in mode line"
  :group 'jp-modeline)

(defface mode-line-read-write-inactive-face
  '((t (:inherit 'mode-line-inactive)))
  "Face for read write indicator in inactive mode line"
  :group 'jp-modeline)

(defface mode-line-filename-face
  '((t (:inherit 'mode-line :weight bold)))
  "Face for filename in mode line"
  :group 'jp-modeline)

(defface mode-line-filename-inactive-face
  '((t (:inherit 'mode-line-inactive :weight bold)))
  "Face for filename in inactive mode line"
  :group 'jp-modeline)

(defface mode-line-process-face
  '((t (:inherit 'mode-line)))
  "Face for process in mode line"
  :group 'jp-modeline)

(defface mode-line-80col-face
  '((t (:inherit 'mode-line)))
  "Face for column number when it's greater than 80"
  :group 'jp-modeline)

;; Mode line setup
(setq-default
 mode-line-format
 '(
   (:eval (jp-modeline-status))
   " "
   ;; Position, including warning for 80 columns
   (:eval (propertize "%4l:" 'face
                      (if (jp-modeline-active-p)
                          'mode-line
                        'mode-line-inactive)))
   (3 (:eval (propertize "%c" 'face
                         (cond
                          ((not (jp-modeline-active-p)) 'mode-line-inactive)
                          ((>= (current-column) 80) 'mode-line-80col-face)
                          (t 'mode-line)))))
   " "
   ;; directory and buffer/file name
   (:eval (propertize (shorten-directory default-directory 10) 'face
                      (if (jp-modeline-active-p)
                          'mode-line
                        'mode-line-inactive)))
   (:eval (propertize (jp-buffer-filename) 'face
                      (if (jp-modeline-active-p)
                          'mode-line-filename-face
                        'mode-line-filename-inactive-face)))
   ;; narrow [default -- keep?]
   " %n "
   ;; major mode
   (:eval (all-the-icons-icon-for-mode
           major-mode
           :height 0.8
           :v-adjust (if (eq major-mode 'emacs-lisp-mode)
                         -0.1
                       0.05)
           :face (if (jp-modeline-active-p)
                     'mode-line
                   'mode-line-inactive)))
   " %["
   (mode-name mode-name)
   "%] "
   ;; vc info
   (:eval (if vc-mode
              (format "◦ %s %s"
                      (all-the-icons-octicon "git-branch" :height 0.8 :v-adjust 0.05)
                      (replace-regexp-in-string "^ Git[:-]" "" vc-mode))
            ""))
   " "
   ;; process
   (:eval (propertize mode-line-process 'face
                      (if (jp-modeline-active-p) 'mode-line-process-face 'mode-line-inactive)))

   ;; global mode string
   (global-mode-string global-mode-string)
   " "))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

(provide 'jp-modeline)
;;; jp-modeline.el ends here
