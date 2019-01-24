;;; jp-org-latex-class.el --- Configuration for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'ox-latex)
(require 's)

(defvar article-serif-font "Georgia")
(defvar article-sans-font "Helvetica")
(defvar article-mono-font "Inconsolata")

(defvar beamer-serif-font article-serif-font)
(defvar beamer-sans-font article-sans-font)
(defvar beamer-mono-font article-mono-font)

(defvar zh-main-font "Adobe Song Std")
(defvar zh-italic-font "Adobe Kaiti Std")
(defvar zh-bold-font "Adobe Heiti Std")

(defun jp-org-latex-class--article-header ()
  (format "
\\documentclass{article}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{color}
\\usepackage[hyperref,x11names,usenames,dvipsnames]{xcolor}
\\hypersetup{colorlinks=true,linkcolor=BlueViolet}
\\usepackage[top=1in,bottom=1in,left=0.8in,right=0.8in]{geometry}
\\usepackage[center,pagestyles]{titlesec}
\\usepackage{indentfirst}
\\usepackage[export]{adjustbox}
\\usepackage{fontspec}
\\setromanfont{%s}
\\setsansfont{%s}
\\setmonofont[Scale=MatchLowercase]{%s}
\\setlength{\\parskip}{0.5\\baselineskip}
\\setlength{\\parindent}{0em}
\\titleformat{\\section}{\\Large\\bfseries}{\\S\\,\\thesection}{1em}{}
\\titleformat{\\subsection}{\\large\\bfseries}{\\S\\,\\thesubsection}{1em}{}
\\titleformat{\\subsubsection}{\\bfseries}{$\\cdot$~\\,\\thesubsubsection}{0.5em}{}
\\newpagestyle{main}{
\\sethead{\\small\\S\\,\\thesection\\quad\\sectiontitle}{}{$\\cdot$~\\thepage~$\\cdot$}
\\setfoot{}{}{}\\headrule}
\\pagestyle{main}

[EXTRA]
"
          article-serif-font
          article-sans-font
          article-mono-font))

(defun jp-org-latex-class--beamer-header ()
  (format "
\\documentclass\[presentation\]\{beamer\}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{fontspec}
\\setromanfont{%s}
\\setsansfont{%s}
\\setmonofont[Scale=MatchLowercase]{%s}
\\AtBeginSection[]{\\begin{frame}<beamer>\\frametitle{Topic}\\tableofcontents[currentsection]\\end{frame}}

[EXTRA]
"
          beamer-serif-font
          beamer-sans-font
          beamer-mono-font))

(defun jp-org-latex-class--zh-extra-header ()
  (format "
\\usepackage{xeCJK}
\\setCJKmainfont[BoldFont=%2$s, ItalicFont=%3$s]{%1$s}
\\setCJKmonofont[Scale=0.9]{%1$s}
\\setCJKfamilyfont{song}[BoldFont=%2$s]{%1$s}
\\setCJKfamilyfont{sf}[BoldFont=%2$s]{%1$s}
\\renewcommand{\\contentsname}{目录}
\\renewcommand{\\listfigurename}{插图目录}
\\renewcommand{\\listtablename}{表格目录}
\\renewcommand{\\refname}{参考文献}
\\renewcommand{\\abstractname}{摘要}
\\renewcommand{\\indexname}{索引}
\\renewcommand{\\tablename}{表}
\\renewcommand{\\figurename}{图}
"
          zh-main-font
          zh-bold-font
          zh-italic-font))

(defun jp-org-latex-class-activate ()
  (let ((a (jp-org-latex-class--article-header))
        (b (jp-org-latex-class--beamer-header))
        (z (jp-org-latex-class--zh-extra-header)))
    (dolist (c `(("article"    ,a)
                 ("article-zh" ,(s-concat a "\n\n" z))
                 ("beamer"     ,b)
                 ("beamer-zh"  ,(s-concat b "\n\n" z))))
      (add-to-list 'org-latex-classes
                   (append c '(("\\section{%s}" . "\\section*{%s}")
                               ("\\subsection{%s}" . "\\subsection*{%s}")
                               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                               ("\\paragraph{%s}" . "\\paragraph*{%s}")
                               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))))

(provide 'jp-org-latex-class)
;;; jp-org-latex-class.el ends here
