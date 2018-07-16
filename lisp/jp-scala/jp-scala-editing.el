;;; jp-scala-editing.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(defun jp-scala-join-line ()
  "Adapt `scala-indent:join-line' to behave more like evil's line join.
`scala-indent:join-line' acts like the vanilla `join-line',
joining the current line with the previous one. The vimmy way is
to join the current line with the next.

Try to move to the subsequent line and then join. Then manually move
point to the position of the join."
  (interactive)
  (let (join-pos)
    (save-excursion
      (goto-char (line-end-position))
      (unless (eobp)
        (forward-line)
        (call-interactively 'scala-indent:join-line)
        (setq join-pos (point))))

    (when join-pos
      (goto-char join-pos))))

;; Automatically insert asterisk in a comment when enabled
(defun jp-scala-newline-and-indent-with-asterisk ()
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

;; Automatically replace arrows with unicode ones when enabled
(defconst scala-unicode-arrows-alist
  '(("=>" . "⇒")
    ("->" . "→")
    ("<-" . "←")))

(defun jp-scala-replace-arrow-at-point ()
  "Replace the arrow before the point (if any) with unicode ones.
An undo boundary is inserted before doing the replacement so that
it can be undone."
  (let* ((end (point))
         (start (max (- end 2) (point-min)))
         (x (buffer-substring start end))
         (arrow (assoc x scala-unicode-arrows-alist)))
    (when arrow
      (undo-boundary)
      (backward-delete-char 2)
      (insert (cdr arrow)))))

(defun jp-scala-gt ()
  "Insert a `>' to the buffer.
If it's part of a right arrow (`->' or `=>'),replace it with the corresponding
unicode arrow."
  (interactive)
  (insert ">")
  (jp-scala-replace-arrow-at-point))

(defun jp-scala-hyphen ()
  "Insert a `-' to the buffer.
If it's part of a left arrow (`<-'),replace it with the unicode arrow."
  (interactive)
  (insert "-")
  (jp-scala-replace-arrow-at-point))


(defun jp-scala-in-multiline-string-p ()
  "Return t if current point is in a multiline string"
  (let* ((state (syntax-ppss))
         (is-string (nth 3 state))
         (start (nth 8 state)))
    (and is-string
         (string= "\"\"\"" (buffer-substring-no-properties start (+ start 3))))))

(defun jp-scala-insert-margin-on-multiline-string ()
  "Insert margin `|' and indent the line if we are in a multiline string.
Only works when previous line contains a margin. Remember to add `stripMargin' to
the end of your string."
  (when (jp-scala-in-multiline-string-p)
    (let ((prev-line (buffer-substring-no-properties
                      (line-beginning-position 0)
                      (line-end-position 0)))
          (current-line (thing-at-point 'line)))
      (when (not (string-match-p "^[[:space:]]*|" current-line))
        (cond
         ;; We are in the middle of multiline string. Use the same indent
         ;; as previous line.
         ((string-match "^[[:space:]]*\\(|[[:space:]]*\\)" prev-line)
          (let ((prefix (match-string-no-properties 0 prev-line)))
            (beginning-of-line)
            (insert prefix)))

         ;; We are in the beginning of a multiline string. scala-mode supports
         ;; indent of multiline strings if it starts with a `|'.
         ((string-match "\"\"\"[[:space:]]*|" prev-line)
          (beginning-of-line)
          (insert "|")
          (indent-according-to-mode)))))))

(defun jp-scala-newline-and-indent-with-pipe ()
  (interactive)
  (jp-scala-newline-and-indent-with-asterisk)
  (jp-scala-insert-margin-on-multiline-string))

(defun jp-scala-insert-margin-befor-ending-parens ()
  (cond
   ((eq 'jp-scala-newline-and-indent-with-pipe this-command)
    (save-excursion
      (forward-line 1)
      (jp-scala-insert-margin-on-multiline-string)))

   ;; Thse two commands are defined in `jp-commands.el', which are
   ;; like `o' and `O' command in vim.
   ((or (eq 'start-newline-after this-command)
        (eq 'start-newline-before this-command))
    (jp-scala-insert-margin-on-multiline-string))))

(provide 'jp-scala-editing)
;;; jp-scala-editing.el ends here
