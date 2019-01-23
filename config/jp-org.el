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
(defconst jp-notes-dir "~/org/notes")


(use-package org
  :straight org-plus-contrib
  :defer t
  :commands (org-capture org-agenda)
  :bind
  (:map
   org-mode-map
   ("RET" . org-return-indent))

  :config
  (progn
    (setq org-directory "~/org")
    (setq org-default-notes-file jp-inbox-org-file)

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

    (setq org-refile-targets '((nil :maxlevel . 9)
                               (jp-gtd-org-file :maxlevel . 2)
                               (jp-someday-org-file :maxlevel . 2)))

    (setq org-startup-folded nil)

    (setq org-log-done 'time)
    (setq org-log-redeadline 'time)
    (setq org-log-reschedule 'time)
    (setq org-goto-interface 'outline-path-completion)
    (setq org-outline-path-complete-in-steps nil)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((dot . t)))

    (defun jp-org-confirm-babel-evaluate (lang body)
      (not (string= lang "dot")))

    (setq org-confirm-babel-evaluate #'jp-org-confirm-babel-evaluate)))


(use-package org-capture
  :after (org)
  :command (org-capture)
  :config
  (progn
    (setq org-capture-templates
          `(("t" "TODO" entry (file org-default-notes-file)
             "* TODO %?\n%u\n")
            ("m" "Meeting" entry (file org-default-notes-file)
             "* Meeting notes for %? :MEETING:\n%t" :clock-in t :clock-resume t)
            ("n" "Next Task" entry (file org-default-notes-file)
             "** NEXT %? \nDEADLINE: %t")))))


(use-package org-agenda
  :after (org)
  :command (org-agenda)
  :config
  (progn
    (setq org-agenda-custom-commands
          '(("k" "Kanban"
             ((todo "NEXT"
                    ((org-agenda-overriding-header "Ready to Start:")))
              (todo "IN-PROGRESS"
                    ((org-agenda-overriding-header "In Progress:")))
              (todo "WAITING"
                    ((org-agenda-overriding-header "Waiting:")))
              (todo "DONE"
                    ((org-agenda-overriding-header "Done:")))))
            ("r" "Daily Review"
             ((agenda "-inbox"
                      ((org-agenda-overriding-header "This Week:")))
              (tags "+inbox"
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-overriding-header "Inbox:")))
              (tags "-inbox/TODO"
                    ((org-agenda-overriding-header "Backlog")))))))

    (setq org-agenda-files (list jp-gtd-org-file
                                 jp-inbox-org-file))

    (setq org-agenda-category-icon-alist
          `(("Emacs" ,(list (all-the-icons-fileicon "emacs" :height 0.8 :v-adjust 0.05))
             nil nil :ascent center)
            ("Projects" ,(list (all-the-icons-octicon "repo" :v-adjust 0.05))
             nil nil :ascent center)
            ("BAU" ,(list (all-the-icons-faicon "tasks" :height 0.8 :v-adjust 0.05))
             nil nil :ascent center)
            ("Inbox" ,(list (all-the-icons-faicon "inbox" :height 0.9 :v-adjust 0.05))
             nil nil :ascent center)))))


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
                                     ("breakanywhere" "true")))

    (defvar en-article "
\\documentclass{article}
\\usepackage{hyperref}
\\usepackage{color}
\\usepackage[hyperref,x11names,usenames,dvipsnames]{xcolor}
\\hypersetup{colorlinks=true,linkcolor=BlueViolet}
\\usepackage{minted}
\\usemintedstyle{emacs}
\\usepackage[top=1in,bottom=1in,left=0.8in,right=0.8in]{geometry}
\\usepackage[center,pagestyles]{titlesec}
\\usepackage{indentfirst}
\\usepackage[export]{adjustbox}
\\usepackage{fontspec}
\\setromanfont{Georgia}
\\setsansfont{Helvetica}
\\setmonofont[Scale=MatchLowercase]{Inconsolata}
\\setlength{\\parskip}{0.5\\baselineskip}
\\setlength{\\parindent}{0em}
\\titleformat{\\section}{\\Large\\bfseries}{\\S\\,\\thesection}{1em}{}
\\titleformat{\\subsection}{\\large\\bfseries}{\\S\\,\\thesubsection}{1em}{}
\\titleformat{\\subsubsection}{\\bfseries}{$\\cdot$~\\,\\thesubsubsection}{0.5em}{}
\\newpagestyle{main}{
\\sethead{\\small\\S\\,\\thesection\\quad\\sectiontitle}{}{$\\cdot$~\\thepage~$\\cdot$}
\\setfoot{}{}{}\\headrule}
\\pagestyle{main}
")

    (defvar en-beamer "
\\documentclass\[presentation\]\{beamer\}
\\usepackage{minted}
\\usemintedstyle{emacs}
\\AtBeginSection[]{\\begin{frame}<beamer>\\frametitle{Topic}\\tableofcontents[currentsection]\\end{frame}}
")

    (defvar zh-preamble "
\\usepackage{xeCJK}
\\setCJKmainfont[BoldFont=Adobe Heiti Std, ItalicFont=Adobe Kaiti Std]{Adobe Song Std}
\\setCJKmonofont[Scale=0.9]{Adobe Song Std}
\\setCJKfamilyfont{song}[BoldFont=Adobe Heiti Std]{Adobe Song Std}
\\setCJKfamilyfont{sf}[BoldFont=Adobe Heiti Std]{Adobe Song Std}
\\renewcommand{\\contentsname}{目录}
\\renewcommand{\\listfigurename}{插图目录}
\\renewcommand{\\listtablename}{表格目录}
\\renewcommand{\\refname}{参考文献}
\\renewcommand{\\abstractname}{摘要}
\\renewcommand{\\indexname}{索引}
\\renewcommand{\\tablename}{表}
\\renewcommand{\\figurename}{图}
")

    (defvar cn-article
      (concat en-article zh-preamble))

    (defvar cn-beamer
      (concat en-beamer zh-preamble))

    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil))

    (add-to-list 'org-latex-classes
                 `("article"
                   ,en-article
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 `("cn-article"
                   ,cn-article
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 `("beamer"
                   ,en-beamer
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 `("cn-beamer"
                   ,cn-beamer
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

(use-package deft
  :straight t
  :commands (deft)
  :config
  (progn
    (setq deft-extensions '("org"))
    (setq deft-default-extension "org")
    (setq deft-directory jp-notes-dir)
    (setq deft-org-mode-title-prefix t)
    (setq deft-use-filter-string-for-filename t)
    (setq deft-use-filename-as-title nil)
    (setq deft-file-naming-rules
          '((noslash . "-")
            (nospace . "-")
            (case-fn . downcase)))))


(provide 'jp-org)
;;; jp-org.el ends here
