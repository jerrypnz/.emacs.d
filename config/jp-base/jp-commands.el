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

;; TODO Look at a hydra for window resizing
(defun increase-window-height (&optional arg)
  "Make the window taller by one line. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window arg))

(defun decrease-window-height (&optional arg)
  "Make the window shorter by one line. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window (- 0 arg)))

(defun decrease-window-width (&optional arg)
  "Make the window narrower by one column. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window (- 0 arg) t))

(defun increase-window-width (&optional arg)
  "Make the window wider by one column. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window arg t))

(defun start-newline-after ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun start-newline-before ()
  (interactive)
  (if (eq (forward-line -1) 0)
      (start-newline-after)
    (progn
      (message "Begin new line at the start")
      (beginning-of-line)
      (newline)
      (forward-line -1))))

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

(provide 'jp-commonds)

