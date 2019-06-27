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
  "Face for flycheck warnings"
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
                                                 ,(+ reserve 1)))))
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
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (with-faicon "question-circle"
                                                      (number-to-string sum))
                                         'face (if .error
                                                   'mode-line-error-face
                                                 'mode-line-warning-face))))
                       (propertize (with-faicon "check-circle" "")
                                   'face 'mode-line-success-face)))
          ('running (propertize (with-faicon "stumbleupon-circle" "")
                                'face 'mode-line-info-face))
          ('no-checker "")
          ('errored (propertize (with-faicon "exclamation-circle" "")
                                'face 'mode-line-error-face))
          ('interrupted  (propertize (with-faicon "pause-circle" "")
                                     'face 'mode-line-info-face)))))


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
             (t (upcase (symbol-name (plist-get sys :name))))))
     " ")))

(defun jp-modeline-filename ()
  (let ((filename (s-concat
                   (when (buffer-file-name)
                     (shorten-directory default-directory 10))
                   (propertize (jp-buffer-filename) 'face
                               (if (jp-modeline-active-p)
                                   'mode-line-filename-face
                                 'mode-line-filename-inactive-face)))))
    (moody-tab (with-mode-icon major-mode filename 0.8 nil) nil 'down)))

(defun jp-modeline-position ()
  `("%4l:"
    (3 ,(s-concat
            (propertize "%c" 'face
                         (cond
                          ((not (jp-modeline-active-p)) 'mode-line-inactive)
                          ((>= (current-column) 80) 'mode-line-80col-face)
                          (t 'mode-line)))))))

(defun jp-modeline-vcinfo ()
  (when vc-mode
    (s-concat
     " "
     (with-octicon "git-branch"
                   (replace-regexp-in-string "^ Git[:-]" "" vc-mode)
                   0.8 0.05)
     " ")))

(defun jp-modeline-major-mode-info ()
  (s-concat" %[" (format-mode-line mode-name) "%] "))

(defun jp-modeline-process-info ()
  (when mode-line-process
    (s-concat
     " "
     (propertize (format-mode-line mode-line-process) 'face
                 (if (jp-modeline-active-p)
                     'mode-line-process-face
                   'mode-line-inactive))
     "  ")))

(defun jp-modeline-narrow-info ()
  (when (buffer-narrowed-p)
    (with-octicon "search" "" 0.8)))

(defun jp-modeline-flycheck-info ()
  (when jp-modeline--flycheck-text
    (let ((x (s-pad-right 4 " " jp-modeline--flycheck-text)))
      (if (jp-modeline-active-p)
          x
        (propertize x 'face 'mode-line-inactive)))))

;; Mode line setup
(defun jp-modeline-setup ()
  ;; Setup flycheck hooks
  (add-hook 'flycheck-status-changed-functions #'jp-modeline--update-flycheck-segment)
  (add-hook 'flycheck-mode-hook #'jp-modeline--update-flycheck-segment)

  (setq-default mode-line-format
                '((:eval
                   (jp-modeline-format
                    ;; Left
                    '((:eval (jp-modeline-status))
                      ;; Position, including warning for 80 columns
                      (:eval (jp-modeline-position))
                      " "
                      (:eval (jp-modeline-filename))
                      " "
                      (:eval (jp-modeline-major-mode-info))
                      (:eval (jp-modeline-vcinfo)))
                    ;; Right
                    '((:eval (jp-modeline-flycheck-info))
                      "  "
                      (:eval (jp-modeline-narrow-info))
                      (:eval (jp-modeline-process-info))
                      (:eval (jp-modeline-encoding))))))))

(provide 'jp-modeline)
;;; jp-modeline.el ends here
