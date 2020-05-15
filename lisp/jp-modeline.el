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
(require 'jp-icons)
(require 's)
(require 'moody)

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

(defface mode-line-info-face
  '((t (:inherit 'mode-line)))
  "Face for flycheck info"
  :group 'jp-modeline)

(defface mode-line-warning-face
  '((t (:inherit 'mode-line)))
  "Face for flycheck warnings"
  :group 'jp-modeline)

(defface mode-line-error-face
  '((t (:inherit 'mode-line)))
  "Face for flycheck errors"
  :group 'jp-modeline)

(defface mode-line-success-face
  '((t (:inherit 'mode-line)))
  "Face for flycheck success"
  :group 'jp-modeline)

(defface header-line-dimmed-face
  '((t (:inherit 'header-line)))
  "Face for flycheck success"
  :group 'jp-modeline)

(defvar jp-modeline-active-window nil)
(defvar jp-modeline-width-threshold 90)

;; Helper functions

(defun jp-modeline-set-active-window (_)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq jp-modeline-active-window (selected-window))))

(add-function :before pre-redisplay-function #'jp-modeline-set-active-window)

(defun jp-modeline-active-p ()
  (eq jp-modeline-active-window (get-buffer-window)))

(defun jp-buffer-filename ()
  (let* ((bufname (buffer-name))
         (name (file-name-nondirectory bufname)))
    (if (eq "" name)
        bufname
      name)))

(defun jp-modeline-long? ()
  (> (window-width) jp-modeline-width-threshold))

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

(defun jp-modeline-format (left right)
  "Return a string of `window-width' length containing LEFT and
RIGHT, aligned respectively."
  (let* ((left (format-mode-line left))
         (right (format-mode-line right))
         (reserve (length right)))
    (when (and (display-graphic-p) (eq 'right (get-scroll-bar-mode)))
      (setq reserve (- reserve 3)))
    (concat
     left
     " "
     (propertize  " "
                  'display `((space :align-to (- (+ right right-fringe right-margin)
                                                 ,reserve)
                                    :ascent 50)))
     right)))

;; Flycheck update function


(defvar flycheck-current-errors)
(declare-function flycheck-count-errors "flycheck")

(defvar-local jp-modeline--flycheck-text nil)

(defun jp-modeline--update-flycheck-segment (&optional status)
  "Update `mood-line--flycheck-text' against the reported flycheck STATUS."
  (setq jp-modeline--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (number-to-string (+ (or .error 0) (or .warning 0) (or .info 0)))))
                             (cond
                              (.error (propertize (s-concat sum " ✗") 'face 'mode-line-error-face))
                              (.warning (propertize (s-concat sum " ✗") 'face 'mode-line-warning-face))
                              (t (propertize (s-concat sum " ⓘ") 'mode-line-info-face)))))
                       (propertize "✓" 'face 'mode-line-success-face)))
          ('running (propertize "…" 'face 'mode-line-info-face))
          ('no-checker "")
          ('errored (propertize "⚠" 'face 'mode-line-error-face))
          ('interrupted  (propertize "⚠" 'face 'mode-line-warning-face)))))

;; Mode-line segments

(defun jp-modeline-status ()
  (let* ((active (jp-modeline-active-p))
         (face (if buffer-read-only
                   (if active 'mode-line-read-only-face 'mode-line-read-only-inactive-face)
                 (if active 'mode-line-read-write-face 'mode-line-read-write-inactive-face)))
         (icon (cond (buffer-read-only    "lock")
                     ((buffer-modified-p) "plus-circle")
                     (t                   "minus-circle"))))
    (s-concat (propertize " " 'face face)
              (all-the-icons-faicon icon :height 0.8 :v-adjust 0.05 :face face)
              (propertize " " 'face face))))

(defun jp-modeline-encoding ()
  (when (jp-modeline-long?)
    (s-concat
     (pcase (coding-system-eol-type buffer-file-coding-system)
       (0 "LF ")
       (1 "CRLF ")
       (2 "CR "))
     (let ((sys (coding-system-plist buffer-file-coding-system)))
       (cond ((memq (plist-get sys :category)
                    '(coding-category-undecided coding-category-utf-8))
              "UTF-8")
             (t (upcase (symbol-name (plist-get sys :name)))))))))

(defun jp-modeline-filename ()
  (let* ((face (if (jp-modeline-active-p) 'mode-line-filename-face 'mode-line-filename-inactive-face))
         (icon-face (unless (jp-modeline-active-p) 'mode-line-inactive))
         (filename (s-concat
                   (when (buffer-file-name)
                     (shorten-directory default-directory 10))
                   (propertize (jp-buffer-filename) 'face face)))
        (icon-size 0.8))
    `(" "
      ,(moody-tab (with-mode-icon major-mode filename icon-size nil icon-face) nil 'down)
      " ")))

(defun jp-modeline-position ()
  `(" %3l:"
    (3 ,(s-concat
            (propertize "%c" 'face
                         (cond
                          ((not (jp-modeline-active-p)) 'mode-line-inactive)
                          ((>= (current-column) 80) 'mode-line-80col-face)
                          (t 'mode-line)))))))

(defun jp-modeline-vc ()
  (when vc-mode
    (s-concat
     " "
     (with-octicon "git-branch"
                   (replace-regexp-in-string "^ Git[:-]" "" vc-mode)
                   0.8 0.05)
     " ")))

(defun jp-modeline-major-mode ()
  (s-concat" %[" (format-mode-line mode-name) "%] "))

(defun jp-modeline-process ()
  (when mode-line-process
    (s-concat
     " "
     (propertize (format-mode-line mode-line-process) 'face
                 (if (jp-modeline-active-p)
                     'mode-line-process-face
                   'mode-line-inactive))
     "  ")))

(defun jp-modeline-narrow ()
  (when (buffer-narrowed-p)
    (with-octicon "search" "" 0.8)))

(defun jp-modeline-flycheck ()
  (when jp-modeline--flycheck-text
    (let ((x (s-pad-right 5 " " jp-modeline--flycheck-text)))
      (if (jp-modeline-active-p)
          x
        (propertize x 'face 'mode-line-inactive)))))

(defun jp-headline-filename ()
  (propertize
   (s-concat "☰" (jp-buffer-filename) " ")
   'face
   (if (jp-modeline-active-p)
       'header-line
     'header-line-dimmed-face)))

(defun jp-headline-position ()
  `(,(propertize " %3l:" 'face 'header-line-dimmed-face)
    (3 ,(propertize "%c" 'face 'header-line-dimmed-face))))

(defun jp-headline-status ()
  (propertize "%*" 'face 'header-line-dimmed-face))

(defun jp-headline-flycheck ()
  (when jp-modeline--flycheck-text
    (if (jp-modeline-active-p)
        jp-modeline--flycheck-text
      (propertize jp-modeline--flycheck-text 'face 'header-line-dimmed-face))))

(defvar jp-modeline-enabled-p nil)

(defun jp-modeline-activate ()
  (if jp-modeline-enabled-p
      (progn
        (setq-default mode-line-format
                      '((:eval
                         (jp-modeline-format
                          ;; Left
                          '((:eval (jp-modeline-status))
                            (:eval (jp-modeline-position))
                            (:eval (jp-modeline-filename))
                            (:eval (jp-modeline-major-mode))
                            (:eval (jp-modeline-vc)))
                          ;; Right
                          '((:eval (jp-modeline-flycheck))
                            (:eval (jp-modeline-narrow))
                            (:eval (jp-modeline-process))
                            (:eval (jp-modeline-encoding)))))))
        (setq-default header-line-format nil))
    (progn
      (setq x-underline-at-descent-line t)
      (setq-default mode-line-format '(""))
      (setq-default header-line-format
                    '((:eval
                       (jp-modeline-format
                        ;; Left
                        '((:eval (jp-headline-filename))
                          (:eval (jp-headline-status)))
                        ;; Right
                        '((:eval (jp-headline-flycheck))
                          (:eval (jp-headline-position))))))))))

;; Mode line setup
(defun jp-modeline-setup ()
  ;; Setup flycheck hooks
  (add-hook 'flycheck-status-changed-functions #'jp-modeline--update-flycheck-segment)
  (add-hook 'flycheck-mode-hook #'jp-modeline--update-flycheck-segment)
  (jp-modeline-activate))

(provide 'jp-modeline)
;;; jp-modeline.el ends here
