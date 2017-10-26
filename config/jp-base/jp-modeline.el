;; Taken from http://amitp.blogspot.co.nz/2011/08/emacs-custom-mode-line.html with
;; some modifications

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
     (cond (buffer-read-only    " RO ")
           ((buffer-modified-p) " ** ")
           (t                   " -- "))
     'face face)))

;; Mode line setup
(setq-default
 mode-line-format
 '(
   (:eval (jp-modeline-status))
   " "
   ;; Position, including warning for 80 columns
   (:propertize "%4l |" face mode-line-position-face)
   (:eval (propertize "%3c " 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   "  "
   ;; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 10))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
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
              (format "| %s"(all-the-icons-octicon "git-branch" :v-adjust 0.05))
            ""))
   (:eval (replace-regexp-in-string "^ Git[:-]" " " vc-mode))
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
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute
 'mode-line nil
 :foreground "gray80" :background "gray20"
 :inverse-video nil
 :box '(:line-width 2 :color "gray20" :style nil))

(set-face-attribute
 'mode-line-inactive nil
 :foreground "gray40" :background "gray20"
 :inverse-video nil
 :box '(:line-width 2 :color "gray20" :style nil))

(set-face-attribute
 'mode-line-read-only-inactive-face nil
 :inherit 'mode-line-face
 :foreground "#4271ae")

(set-face-attribute
 'mode-line-read-only-face nil
 :inherit 'mode-line-face
 :foreground "gray80" :background "#4271ae"
 :box '(:line-width 2 :color "#4271ae"))

(set-face-attribute
 'mode-line-read-write-inactive-face nil
 :inherit 'mode-line-face
 :foreground "#c82829")

(set-face-attribute
 'mode-line-read-write-face nil
 :inherit 'mode-line-face
 :foreground "gray80" :background "#c82829"
 :box '(:line-width 2 :color "#c82829"))

(set-face-attribute
 'mode-line-folder-face nil
 :inherit 'mode-line-face
 :foreground "gray60")

(set-face-attribute
 'mode-line-filename-face nil
 :inherit 'mode-line-face
 :foreground "#eab700"
 :weight 'bold)

(set-face-attribute
 'mode-line-position-face nil
 :inherit 'mode-line-face :height 100)

(set-face-attribute
 'mode-line-process-face nil
 :inherit 'mode-line-face
 :foreground "#718c00")

(set-face-attribute
 'mode-line-80col-face nil
 :inherit 'mode-line-position-face
 :foreground "black" :background "#eab700")

(provide 'jp-modeline)
