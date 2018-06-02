;;; jp-org.el --- Configuration for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(defconst jp-work-org-file "~/org/work.org")
(defconst jp-private-org-file "~/org/private.org")
(defconst jp-inbox-org-file "~/org/inbox.org")
(defconst jp-diary-org-fle "~/org/diary.org")
(defconst jp-notes-dir "~/org/notes")


(use-package org
  :straight org-plus-contrib
  :defer t
  :bind
  (("M-m c" . org-capture)
   ("M-m a" . org-agenda)
   :map org-mode-map
   ("RET" . org-return-indent))

  :config
  (progn
    (setq org-directory "~/org")
    (setq org-default-notes-file jp-inbox-org-file)

    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))

    (setq org-todo-state-tags-triggers
          '(("CANCELLED" ("CANCELLED" . t))
            ("WAITING" ("WAITING" . t))
            ("HOLD" ("WAITING") ("HOLD" . t))
            (done ("WAITING") ("HOLD"))
            ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
            ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
            ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

    (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

    (setq org-capture-templates
          `(("t" "TODO" entry (file org-default-notes-file)
             "* TODO %?\n%u\n" :clock-in t :clock-resume t)
            ("m" "Meeting" entry (file org-default-notes-file)
             "* Meeting notes for %? :MEETING:\n%t" :clock-in t :clock-resume t)
            ("d" "Diary" entry (file+datetree ,jp-diary-org-fle)
             "* %?\n%U\n" :clock-in t :clock-resume t)
            ("i" "Idea" entry (file org-default-notes-file)
             "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
            ("n" "Next Task" entry (file org-default-notes-file)
             "** NEXT %? \nDEADLINE: %t")))

    (setq org-agenda-custom-commands
          '(("a" "Agenda"
             ((agenda "-REFILE")
              (alltodo "-REFILE")
              (tags "REFILE"
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-overriding-header "Inbox:")))))))

    (setq org-agenda-files (list jp-work-org-file
                                 jp-private-org-file
                                 jp-inbox-org-file))

    (setq org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9)))

    (setq org-startup-folded nil)))


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

    (defvar en-article "
\\documentclass{scrartcl}
\\usepackage{hyperref}
\\usepackage{color}
\\usepackage[hyperref,x11names,usenames,dvipsnames]{xcolor}
\\hypersetup{colorlinks=true,linkcolor=BlueViolet}
\\usepackage{minted}
\\usepackage[top=1in,bottom=1in,left=0.8in,right=0.8in]{geometry}
\\usepackage[center,pagestyles]{titlesec}
\\usepackage{indentfirst}
\\usepackage[export]{adjustbox}
\\usemintedstyle{emacs}
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
  :bind ("M-m n" . deft)
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
