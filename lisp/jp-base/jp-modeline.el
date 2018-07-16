;;; jp-modeline.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:
;;
;; Taken from http://amitp.blogspot.co.nz/2011/08/emacs-custom-mode-line.html with
;; some modifications
;;
;;; Code:

(autoload 'all-the-icons-icon-for-mode "all-the-icons")
(autoload 'all-the-icons-octicon "all-the-icons")

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
                 (if active 'mode-line-read-write-face 'mode-line-read-write-inactive-face))))
    (propertize
     (cond (buffer-read-only    " R ")
           ((buffer-modified-p) " * ")
           (t                   " - "))
     'face face)))

(defun jp-buffer-filename ()
  (let* ((bufname (buffer-name))
         (name (file-name-nondirectory bufname)))
    (if (eq "" name)
        bufname
      name)))

;; Mode line setup
(setq-default
 mode-line-format
 '(
   (:eval (jp-modeline-status))
   " "
   ;; Position, including warning for 80 columns
   (:eval (propertize "%4l |" 'face
                      (if (jp-modeline-active-p)
                          'mode-line-position-face
                        'mode-line-position-inactive-face)))
   (:eval (propertize "%3c " 'face
                      (cond
                       ((not (jp-modeline-active-p)) 'mode-line-position-inactive-face)
                       ((>= (current-column) 80) 'mode-line-80col-face)
                       (t 'mode-line-position-face))))
   "  "
   ;; directory and buffer/file name
   (:eval (propertize (shorten-directory default-directory 10) 'face
                      (if (jp-modeline-active-p)
                          'mode-line-folder-face
                        'mode-line-folder-inactive-face)))
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
              (format "| %s %s"
                      (all-the-icons-octicon "git-branch" :height 0.8 :v-adjust 0.05)
                      (replace-regexp-in-string "^ Git[:-]" "" vc-mode))
            ""))
   "   "
   ;; process
   (:propertize mode-line-process
                face mode-line-process-face)
   ;; global mode string
   (global-mode-string global-mode-string)
   "    "))

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
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-read-only-inactive-face)
(make-face 'mode-line-read-write-face)
(make-face 'mode-line-read-write-inactive-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-folder-inactive-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-filename-inactive-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-position-inactive-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(defvar jp-modeline--read-only-color "#2257A0")
(defvar jp-modeline--read-write-color "#BF616A")
(defvar jp-modeline--filename-color "#D08770")

(let ((bg (face-attribute 'mode-line :background)))
  (set-face-attribute
   'mode-line nil
   :inverse-video nil
   :box `(:line-width 2 :color ,bg :style nil)))

(let ((bg (face-attribute 'mode-line-inactive :background)))
  (set-face-attribute
   'mode-line-inactive nil
   :inverse-video nil
   :box `(:line-width 2 :color ,bg :style nil)))

(set-face-attribute
 'mode-line-read-only-inactive-face nil
 :inherit 'mode-line-inactive
 :foreground jp-modeline--read-only-color)

(set-face-attribute
 'mode-line-read-only-face nil
 :inherit 'mode-line
 :background jp-modeline--read-only-color
 :box `(:line-width 2 :color ,jp-modeline--read-only-color))

(set-face-attribute
 'mode-line-read-write-inactive-face nil
 :inherit 'mode-line-inactive
 :foreground jp-modeline--read-write-color)

(set-face-attribute
 'mode-line-read-write-face nil
 :inherit 'mode-line
 :background jp-modeline--read-write-color
 :box `(:line-width 2 :color ,jp-modeline--read-write-color))

(set-face-attribute
 'mode-line-folder-face nil
 :inherit 'mode-line)

(set-face-attribute
 'mode-line-folder-inactive-face nil
 :inherit 'mode-line-inactive)

(set-face-attribute
 'mode-line-filename-face nil
 :inherit 'mode-line
 :foreground jp-modeline--filename-color
 :weight 'bold)

(set-face-attribute
 'mode-line-filename-inactive-face nil
 :inherit 'mode-line-inactive
 :weight 'bold)

(set-face-attribute
 'mode-line-position-face nil
 :inherit 'mode-line)

(set-face-attribute
 'mode-line-position-inactive-face nil
 :inherit 'mode-line-inactive)

(set-face-attribute
 'mode-line-process-face nil
 :inherit 'mode-line
 :foreground "#718c00")

(set-face-attribute
 'mode-line-80col-face nil
 :inherit 'mode-line-position-face
 :foreground "black" :background jp-modeline--filename-color)

(provide 'jp-modeline)
;;; jp-modeline.el ends here
