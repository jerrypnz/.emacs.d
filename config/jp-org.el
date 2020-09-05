;;; jp-org.el --- Configuration for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(defconst jp-gtd-org-file "~/org/gtd.org")
(defconst jp-inbox-org-file "~/org/inbox.org")
(defconst jp-someday-org-file "~/org/someday.org")
(defconst jp-work-notes "~/org/work-notes.org")
(defconst jp-study-notes "~/org/study-notes.org")

(use-package org
  :straight org-plus-contrib
  :defer t
  :bind
  (:map
   org-mode-map
   ("RET" . org-return-indent))

  :mode-hydra
  (org-mode
   ("Navigation"
    (("n" outline-next-visible-heading "next heading" :exit nil)
     ("p" outline-previous-visible-heading "previous heading" :exit nil)
     ("u" outline-up-heading "up level")
     ("g" org-goto "goto"))))

  :config
  (progn
    (setq org-directory "~/org")
    (setq org-default-notes-file jp-inbox-org-file)

    (setq org-refile-targets '((nil :maxlevel . 9)
                               (jp-work-notes :maxlevel . 4)
                               (jp-study-notes :maxlevel . 4)))

    (setq org-refile-use-outline-path 'file)

    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "|" "DONE(d)")
            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

    (setq org-todo-state-tags-triggers
          '(("CANCELLED" ("CANCELLED" . t))
            ("WAITING" ("WAITING" . t))
            ("HOLD" ("WAITING") ("HOLD" . t))
            (done ("WAITING") ("HOLD"))
            ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
            ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
            ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

    (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

    (setq org-startup-folded nil)

    (setq org-log-done 'time)
    (setq org-log-redeadline 'time)
    (setq org-log-reschedule 'time)
    (setq org-goto-interface 'outline-path-completion)
    (setq org-outline-path-complete-in-steps nil)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((dot . t)
       (emacs-lisp . t)
       (plantuml . t)))

    (defun jp-org-confirm-babel-evaluate (lang body)
      (not (member lang '("dot" "plantuml" "spark-shell" "amm" "elisp"))))

    (setq org-confirm-babel-evaluate #'jp-org-confirm-babel-evaluate)))

(use-package org-capture
  :after (org)
  :commands (org-capture)
  :config
  (progn
    (setq org-capture-templates
          `(("t" "TODO" entry (file org-default-notes-file)
             "* TODO %?\n%u\n")
            ("p" "Project" entry (file org-default-notes-file)
             "* %? [%] :PROJECT:\n%u\n")
            ("m" "Meeting" entry (file org-default-notes-file)
             "* Meeting notes for %? :MEETING:\n%t" :clock-in t :clock-resume t)
            ("n" "Notes" entry (file org-default-notes-file)
             "* %? \n%t")))))

(use-package jp-org-refile
  :after (org)
  :config
  (setq jp-org-refile-contexts
        `((org-entry-is-todo-p
           (jp-gtd-org-file :maxlevel . 3)
           (jp-someday-org-file :maxlevel . 3))
          ((lambda () (member "PROJECT" (org-get-tags)))
           (jp-gtd-org-file :regexp . "Projects")
           (jp-someday-org-file :regexp . "Projects"))
          ((lambda () (member "MEETING" (org-get-tags)))
           (jp-work-notes :regexp . "Meeting Notes")))))

(use-package org-agenda
  :after (org)
  :commands (org-agenda)
  :config
  (progn
    (setq org-agenda-restore-windows-after-quit t)
    (setq org-agenda-window-setup 'only-window)

    (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)

    (setq org-agenda-custom-commands
          '(("k" "Kanban"
             ((tags "-someday-inbox/NEXT"
                    ((org-agenda-overriding-header "Ready to Start:")))
              (tags "-someday-inbox+TODO=\"IN-PROGRESS\""
                    ((org-agenda-overriding-header "In Progress:")))
              (tags "-someday-inbox/WAITING"
                    ((org-agenda-overriding-header "Waiting:")))
              (tags "-someday-inbox+CLOSED>=\"<-3d>\"/DONE"
                    ((org-agenda-overriding-header "Done:")))
              (agenda "-someday-inbox"
                      ((org-agenda-overriding-header "Calendar:")))))
            ("r" "Daily Review"
             ((tags "+inbox"
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-overriding-header "Inbox:")))
              (tags "-someday-inbox/TODO"
                    ((org-agenda-overriding-header "Backlog:")))))
            ("s" "Someday"
             ((tags "+someday/TODO"
                    ((org-agenda-overriding-header "Maybe Someday:")))))))

    (setq org-agenda-files (list jp-gtd-org-file
                                 jp-inbox-org-file
                                 jp-someday-org-file))

    (setq org-agenda-category-icon-alist
          `(("Emacs" ,(list (all-the-icons-fileicon "emacs" :height 0.8 :v-adjust 0.05))
             nil nil :ascent center)
            ("Projects" ,(list (all-the-icons-octicon "repo" :v-adjust 0.05))
             nil nil :ascent center)
            ("BAU" ,(list (all-the-icons-faicon "tasks" :height 0.8 :v-adjust 0.05))
             nil nil :ascent center)
            ("Inbox" ,(list (all-the-icons-faicon "inbox" :height 0.9 :v-adjust 0.05))
             nil nil :ascent center)
            ("Personal" ,(list (all-the-icons-faicon "user" :height 0.95 :v-adjust 0.05))
             nil nil :ascent center)
            ("Reading" ,(list (all-the-icons-faicon "book" :height 0.95 :v-adjust 0.05))
             nil nil :ascent center)))

    (setq org-agenda-sorting-strategy
          '((agenda habit-down time-up priority-down category-keep)
            (todo category-keep timestamp-down)
            (tags category-keep timestamp-down)
            (search category-keep)))))

(use-package jp-org-agenda
  :commands (major-mode-hydras/org-agenda-mode/body))

(use-package org-archive
  :after (org)
  :commands (org-archive-subtree)
  :config
  (progn
    ;; Found here: https://github.com/Fuco1/.emacs.d/blob/master/files/org-defs.el#L1795
    (defadvice org-archive-subtree (around fix-hierarchy activate)
      (let* ((fix-archive-p (and (not current-prefix-arg)
                                 (not (use-region-p))))
             (afile (org-extract-archive-file (org-get-local-archive-location)))
             (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
        ad-do-it
        (when fix-archive-p
          (with-current-buffer buffer
            (goto-char (point-max))
            (while (org-up-heading-safe))
            (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
                   (path (and olpath (split-string olpath "/")))
                   (level 1)
                   tree-text)
              (when olpath
                (org-mark-subtree)
                (setq tree-text (buffer-substring (region-beginning) (region-end)))
                (let (this-command) (org-cut-subtree))
                (goto-char (point-min))
                (save-restriction
                  (widen)
                  (-each path
                    (lambda (heading)
                      (if (re-search-forward
                           (rx-to-string
                            `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                          (org-narrow-to-subtree)
                        (goto-char (point-max))
                        (unless (looking-at "^")
                          (insert "\n"))
                        (insert (make-string level ?*)
                                " "
                                heading
                                "\n"))
                      (cl-incf level)))
                  (widen)
                  (org-end-of-subtree t t)
                  (org-paste-subtree level tree-text))))))))))


(use-package ox-gfm
  :straight t
  :after (org))

(use-package ox-html
  :after (org)
  :config
  (progn
    (setq org-html-htmlize-output-type 'css)
    (setq org-html-postamble nil)))

(use-package ox-latex
  :after (org)
  :config
  (progn
    (setq org-latex-compiler "xelatex")
    ;; for multiple passes
    (setq org-latex-pdf-process
          '("xelatex -shell-escape -interaction nonstopmode %f"
            "xelatex -shell-escape -interaction nonstopmode %f"
            "xelatex -shell-escape -interaction nonstopmode %f"
            "xelatex -shell-escape -interaction nonstopmode %f"
            "rm -f %o/%b*.vrb"))

    ;; Stop org from keep the tables centered
    (setq org-latex-tables-centered nil)
    ;; Use `minted' for syntax highlighting
    (setq org-latex-listings 'minted)
    (setq org-latex-minted-options '(("breaklines" "true")
                                     ("breakanywhere" "true")
                                     ("style" "tango")
                                     ("frame" "single")))
    (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))))

(use-package jp-org-latex-class
  :after (ox-latex)
  :config
  (progn
    (jp-org-latex-class-activate)))

(use-package verb
  :straight t
  :after (org)
  :mode-hydra
  (org-mode
   ("Verb Requests"
    (("rr" verb-send-request-on-point-other-window "send")
     ("re" verb-export-request-on-point-curl "export")
     ("rs" verb-set-var "set var")))))

(provide 'jp-org)
;;; jp-org.el ends here
