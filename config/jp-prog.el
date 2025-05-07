;;; jp-prog.el --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:


;;; Clojure

(eval-when-compile
  (require 'use-package)
  (require 'major-mode-hydra))

(use-package clojure-mode
  :straight t

  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode))

  :config
  (progn

    ;; If put in `define-clojure-indent', it will fail with a "wrong
    ;; type argument: listp, 1" error the first time a clj buffer is
    ;; opened.
    (put-clojure-indent '-> 1)
    (put-clojure-indent '->> 1)

    (add-to-list 'clojure-align-cond-forms "more-of")

    (define-clojure-indent
      ;; core.match
      (match 1)
      ;; expectations
      (expect 0)
      (expect-let 1)
      (more-of 1)
      ;; clj-time
      (do-at 1)
      ;; custom core.async macros
      (go? 0)
      (go?>x 1)
      (go?> 1)
      (go-loop? 1)
      (go-handle 1)
      (thread? 0)
      (thread?> 1)
      (thread?>x 1)
      (some-> 1)
      (some->> 1)
      (->application 1)
      (->server 1)
      ;; metrics-clojure
      (time! 1)
      (with-logging-context 1)
      ;; hiccup
      (xhtml 1)
      (html5 1)))

  :pretty-hydra
  ((:color teal :quite-key "q" :title "Edit Clojure Source")
   ("Basic"
    (("SPC" clojure-align "align")
     ("p" clojure-cycle-privacy "cycle privacy")
     ("f" clojure-cycle-if "cycle if/if-not")
     ("w" clojure-cycle-when "cycle when/when-not")
     (":" clojure-toggle-keyword-string "toggle keyword/string")
     ("!" clojure-cycle-not "cycle not"))
    "Let"
    (("ll" (clojure-introduce-let 1) "introduce let")
     ("lm" clojure-move-to-let "move to let")
     ("lf" clojure-let-forward-slurp-sexp "forward slurp into")
     ("lb" clojure-let-backward-slurp-sexp "backward slurp into"))
    "Namespaces"
    (("nn" clojure-insert-ns-form "insert ns form")
     ("nh" clojure-insert-ns-form-at-point "insert ns form here")
     ("np" clojure-update-ns "update ns form")
     ("ns" clojure-sort-ns "sort ns"))
    "Collections"
    (("cl" clojure-convert-collection-to-list "to list")
     ("cq" clojure-convert-collection-to-quoted-list "to quoted list")
     ("cm" clojure-convert-collection-to-map "to map")
     ("cv" clojure-convert-collection-to-vector "to vector")
     ("cs" clojure-convert-collection-to-set "to set"))
    "Threading"
    (("tt" clojure-thread "thread once")
     ("tf" clojure-thread-first-all "thread all ->")
     ("tl" clojure-thread-last-all "thread all ->>")
     ("tw" clojure-unwind "unwind once")
     ("tu" clojure-unwind-all "unwind all"))))

  :mode-hydra
  ((clojure-mode clojurescript-mode clojurec-mode)
   ("Clojure"
    ((">" clojure-thread-first-all "thread-first")
     ("<" clojure-thread-last-all "thread-last")
     ("u" clojure-unwind-all "thread-unwind")
     (":" clojure-toggle-keyword-string "toggle keyword/string")
     ("SPC" clojure-align "align")
     ("TAB" clojure-mode-hydra/body "more...")))))

(use-package cider
  :straight t

  :config
  (progn
    ;; We want the clojure-lsp version of these
    (define-key cider-mode-map (kbd "M-.") nil)
    (define-key cider-mode-map (kbd "M-,") nil)
    (define-key cider-mode-map (kbd "M-TAB") nil)

    ;; REPL history file
    (setq cider-repl-history-file "~/.emacs.d.cache/cider-history")
    ;; Don't prompt for symbol by default
    (setq cider-prompt-for-symbol nil)
    ;; nice pretty printing
    (setq cider-repl-use-pretty-printing t)
    ;; nicer font lock in REPL
    (setq cider-repl-use-clojure-font-lock t)
    ;; never ending REPL history
    (setq cider-repl-wrap-history t)
    ;; looong history
    (setq cider-repl-history-size 3000)
    (setq cider-repl-display-help-banner nil)
    ;; eldoc for clojure
    (add-hook 'cider-mode-hook #'eldoc-mode)
    ;; smartparens mode for the REPL
    (add-hook 'cider-repl-mode-hook #'smartparens-mode)
    ;; error buffer not popping up
    (setq cider-show-error-buffer nil)
    (setq cider-print-fn 'fipp))

  :mode-hydra
  ((clojure-mode clojurec-mode clojurescript-mode)
   ("Quick Action"
    (("t" cider-test-run-ns-tests "ns")
     ("T" cider-test-run-loaded-tests "loaded")
     ("F" cider-test-rerun-failed-tests "failed"))
    "Find & Goto"
    (("n" cider-find-ns "ns")
     ("D" cider-doc "doc"))
    "Connection"
    (("j" cider-jack-in "cider jack in")
     ("J" cider-jack-in-clojurescript "cider jack in cljs")
     ("C" cider-connect "cider connect")
     ("R" cider-restart "cider restart")
     ("Q" cider-quit "cider disconnect"))
    "Load & Eval"
    (("ll" cider-load-buffer "load buffer")
     ("lf" cider-load-file "load file")
     ("la" cider-load-all-project-ns "load all ns")
     ("lr" cider-ns-refresh "reload")
     ("es" cider-repl-set-ns "set repl ns")
     ("ee" cider-eval-last-sexp-to-repl "eval last")
     ("ef" cider-eval-defun-at-point "eval defun")
     ("ei" cider-inspect-last-result "inspect last result")
     ("ed" (cider-eval-defun-at-point t) "debug defun")
     ("ei" cider-interrupt "interrupt"))))

  :mode-hydra
  (cider-repl-mode
   ("Connection"
    (("R" cider-restart "restart")
     ("Q" cider-quit "disconnect"))
    "Load & Eval"
    (("f" cider-load-file "load file")
     ("a" cider-load-all-project-ns "load all ns")
     ("r" cider-ns-refresh "reload"))
    "REPL"
    (("s" cider-repl-set-ns "set-repl-ns")
     ("i" cider-interrupt "interrupt")
     ("c" cider-repl-clear-buffer "clear"))
    "Find"
    (("d" cider-doc "doc")))))

;;(use-package jp-counsel-cider
;;  :after (cider)
;;
;;  :mode-hydra
;;  ((clojure-mode cider-repl-mode clojurec-mode)
;;   ("Find & Goto"
;;    (("ga" jp-counsel-cider-apropos "apropos"))))
;;
;;  :mode-hydra
;;  (cider-repl-mode
;;   ("REPL"
;;    (("h" jp-counsel-cider-repl-history "search-history")
;;     ("H" cider-repl-history "show-history")))))

(use-package cider-macroexpansion
  :after (cider)

  :mode-hydra
  ((clojure-mode cider-repl-mode)
   ("Load & Eval"
    (("ex" cider-macroexpand-1 "macroexpand-1")
     ("eX" cider-macroexpand-all "macroexpand-all")))))

(use-package inf-clojure
  :straight t

  :mode-hydra
  (clojurescript-mode
   ("Connection"
    (("j" (progn (inf-clojure) (inf-clojure-minor-mode +1)) "inf-clj jack in")
     ("J" (progn (inf-clojure "planck -d") (inf-clojure-minor-mode +1)) "jack in (planck)")
     ("C" (progn (inf-clojure-connect) (inf-clojure-minor-mode +1)) "inf-clj connect")
     ("Q" inf-clojure-quit "inf-clj disconnect"))
    "Load & Eval"
    (("ll" inf-clojure-eval-buffer "load buffer")
     ("lf" inf-clojure-load-file "load file")
     ("lr" inf-clojure-reload "reload")
     ("es" inf-clojure-set-ns "set repl ns")
     ("ee" inf-clojure-eval-last-sexp "eval last")
     ("ef" inf-clojure-eval-defun "eval defun")
     ("ex" inf-clojure-macroexpand "macroexpand"))
    "Find & Goto"
    (("d" inf-clojure-show-var-documentation "doc")
     ("ga" inf-clojure-apropos "apropos")))))

;; (use-package flycheck-clj-kondo
;;   :straight t
;;   :after (clojure-mode)
;;   :config
;;   (require 'flycheck-clj-kondo)

;;   (defun jp-clj-kondo-lint-all ()
;;     (interactive)
;;     (let* ((proj-root (projectile-project-root))
;;            (proj-file (expand-file-name "project.clj" proj-root))
;;            (clj-kondo-dir (expand-file-name ".clj-kondo" proj-root)))
;;       (if (not (file-exists-p proj-file))
;;           (user-error "Not a leiningen project: %s" proj-root)
;;         (make-directory clj-kondo-dir t)
;;         (message "Running 'lein classpath' to get classpath")
;;         (let ((classpath (shell-command-to-string "lein classpath 2>/dev/null")))
;;           (compile (format "clj-kondo --lint %s --cache" (s-trim classpath))))))))



;;; Scala



(use-package scala-mode
  :straight t
  :mode ("\\.scala\\'" "\\.sbt\\'")
  :defer t
  :init
  (progn
    (dolist (ext '(".cfe" ".cfs" ".si" ".gen" ".lock"))
      (add-to-list 'completion-ignored-extensions ext)))
  :config
  (progn
    (require 'jp-scala-editing)
    (add-hook 'scala-mode-hook
              (lambda ()
                ;; Add the hook to the end of the list and use buffer local hooks
                ;; because we want to enable this hook only in scala-mode.
                (add-hook 'post-command-hook 'jp-scala-insert-margin-befor-ending-parens t t)))

    (define-key scala-mode-map (kbd "RET") 'jp-scala-newline-and-indent-with-pipe)
    ;;(define-key scala-mode-map (kbd ">") 'jp-scala-gt)
    ;;(define-key scala-mode-map (kbd "-") 'jp-scala-hyphen)
    (define-key scala-mode-map (kbd "M-j") 'jp-scala-join-line)

    ;; Compatibility with `aggressive-indent'
    (setq scala-indent:align-forms nil
          scala-indent:align-parameters nil
          scala-indent:default-run-on-strategy scala-indent:operator-strategy)))

(use-package sbt-mode
  :straight t
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package bloop
  :straight (:host github :type git :repo "tarao/emacs-bloop")
  :defer nil
  :mode-hydra
  (scala-mode
   ("Quick Action"
    (("b" bloop-compile "build")
     ("t" bloop-test-only "test")
     ("T" bloop-test "test all")))))

(use-package ammonite-term-repl
  :straight t
  :commands (run-ammonite)
  :custom
  (ammonite-term-repl-auto-detect-predef-file nil)
  (ammonite-term-repl-program-args '("--class-based")))

(use-package ob-ammonite
  :straight t)



;;; Go


(eval-when-compile
  (require 'use-package))

(use-package go-mode
  :straight t
  :mode "\\.go\\'")

(use-package jp-go-tests
  :after (go-mode)
  :mode-hydra
  (go-mode
   ("test"
    (("tt" jp-go-run-test-current-function "current function")
     ("ts" jp-go-run-test-current-suite "current suite")
     ("tp" jp-go-run-package-tests "package")
     ("tp" jp-go-run-package-tests-nested "package nested")))))

(use-package jp-go-play
  :commands (go-play))


;;; Rust


(use-package toml-mode
  :straight t
  :mode "\\.toml\\'")

(use-package rust-mode
  :straight t
  :mode "\\.rs\\'")

;; Add keybindings for interacting with Cargo
(use-package cargo
  :preface
  (progn
    (defvar cargo-hydra--title)
    (setq cargo-hydra--title (with-octicon "package" "Cargo")))
  :straight t

  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Cargo")
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.25)))

  :pretty-hydra
  ((:title cargo-hydra--title :quit-key "q")
   ("Packages"
    (("a" cargo-process-add "add")
     ("u" cargo-process-update "update")
     ("x" cargo-process-rm "remove")
     ("U" cargo-process-upgrade "upgrade"))
    "Actions"
    (("b" cargo-process-build "build")
     ("r" cargo-process-run "run")
     ("c" cargo-process-check "check")
     ("f" cargo-process-format "format")
     ("s" cargo-process-search "search"))
    "Tests"
    (("t" cargo-process-current-file-tests "test")
     ("T" cargo-process-test "test all"))))

  :mode-hydra
  (rust-mode
   ("Quick Action"
    (("b" cargo-process-build "build")
     ("t" cargo-process-current-file-tests "test")
     ("T" cargo-process-test "test all")
     ("R" cargo-process-run "run")
     ("C" cargo-hydra/body "cargo...")))))


;;; Other major modes


(use-package protobuf-mode
  :straight (:host github :repo "emacsmirror/protobuf-mode")
  :mode ("\\.proto\\'"))

(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'")

(use-package puppet-mode
  :straight t
  :mode (("\\.pp\\'"  . puppet-mode)))

(use-package hcl-mode
  :straight t
  :mode (("\\.hcl\\'"  . hcl-mode)))

(use-package php-mode
  :straight t
  :mode ("\\.php\\'"))

(use-package yaml-mode
  :straight t
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(use-package cql-mode
  :straight t
  :mode ("\\.cql\\'" "\\.hcql\\'"))

(use-package lua-mode
  :straight (:host github :repo "immerrr/lua-mode")
  :mode ("\\.lua\\'")
  :interpreter "lua")

(use-package fennel-mode
  :straight (:host gitlab :repo "technomancy/fennel-mode")
  :mode ("\\.fnl\\'")
  :config
  (progn
    (put 'global 'fennel-indent-function 1)
    (put 'if 'fennel-indent-function 1)))

(use-package groovy-mode
  :straight t
  :mode ("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" "Jenkinsfile")
  :interpreter "groovy")

(use-package mmm-jinja2
  :straight t
  :config
  (progn
    (setq mmm-global-mode 'maybe)
    (dolist (x '(("edn" . clojure-mode)
                 ("yaml" . yaml-mode)
                 ("sh" . sh-mode)))
      (let ((pattern (concat "\\." (car x) "\\.j2\\'")))
        (add-to-list 'auto-mode-alist `(,pattern . ,(cdr x)))
        (mmm-add-mode-ext-class (cdr x) pattern 'jinja2)))))

(use-package ess
  :straight t)

(use-package graphviz-dot-mode
  :straight t
  :mode ("\\.dot\\'")
  :config (setq graphviz-dot-indent-width 2))

(use-package typescript-mode
  :straight t
  :mode ("\\.ts\\'"))

(use-package company-graphviz-dot
  :after (company graphviz-dot-mode))



;; LSP


(use-package lsp-mode
  :straight t
  :hook (((go-mode
           python-mode
           rust-mode
           java-mode
           scala-mode
           clojure-mode
           clojurescript-mode
           typescript-mode
           ) . lsp-deferred)
         (lsp-mode . lsp-lens-mode))

  :bind
  (:map lsp-mode-map
   ("M-." . lsp-find-definition))

  :config
  (setq lsp-completion-provider :none)
  (setq lsp-auto-execute-action nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]cdk\\.out\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]__pycache__\\'"))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil))

(use-package lsp-clojure
  :after (lsp-mode))

(use-package dap-mode
  :straight t
  :after (lsp-mode)
  :hook ((lsp-mode . dap-mode)
         (lsp-mode . dap-ui-mode))
  :config
  (progn
    (require 'ansi-color)
    (defun colorize-compilation-buffer ()
      (toggle-read-only)
      (ansi-color-apply-on-region compilation-filter-start (point))
      (toggle-read-only))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))

(use-package lsp-java
  :straight t
  :after (lsp-mode)
  :config
  (setq lsp-java-server-install-dir (expand-file-name "~/.jdt.ls/")
        lsp-java-workspace-dir (expand-file-name "~/.jdt-workspace/")
        lsp-java-workspace-cache-dir (expand-file-name ".cache/" lsp-java-workspace-dir)
        lsp-java-maven-download-sources t
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-format-settings-profile "GoogleStyle"))

(use-package dap-java
  :after (dap-mode)
  :config
  (setq dap-java-test-runner (expand-file-name "~/.jdt.ls/test-runner/junit-platform-console-standalone.jar")))

(use-package lsp-rust
  :after (lsp-mode)
  :init
  (progn
    (defvar lsp-rust-server)
    (setq lsp-rust-server 'rust-analyzer))
  :custom
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-max-inlay-hint-length 16)
  (lsp-rust-analyzer-use-client-watching t)
  (lsp-rust-analyzer-cargo-watch-enable t)
  (lsp-rust-analyzer-cargo-watch-command "clippy"))

;; (use-package lsp-metals
;;   :straight t
;;   :after (lsp-mode)
;;   :custom
;;   (lsp-metals-maven-script "/usr/bin/mvn")
;;   (lsp-metals-sbt-script "/usr/bin/sbt")
;;   ;;(lsp-metals-bloop-version "1.4.2")
;;   )

;; (use-package lsp-python-ms
;;   :straight t
;;   :after (lsp-mode)
;;   :config
;;   (setq lsp-python-ms-auto-install-server t))

(use-package lsp-pyright
  :straight t
  :after (lsp-mode))

(use-package jp-lsp-hydra)



;;; Copilot


(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
         ("C-<tab>" . jp-copilot-accept-or-indent))
  :config
  (defun jp-copilot-accept-or-indent ()
    (interactive)
    (or (call-interactively #'copilot-accept-completion)
        (call-interactively #'indent-for-tab-command))))

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode)
  :config
  (setq copilot-chat-model "claude-3.5-sonnet"))

(use-package password-store
  :straight t)

(use-package aidermacs
  :preface (progn
             (defvar aidermacs-hydra--title)
             (setq aidermacs-hydra--title (with-octicon "gear" "Aidermacs")))

  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :pretty-hydra ((:color teal :quite-key "q" :title aidermacs-hydra--title)
                 ("Session"
                  (("a" aidermacs-run "Start/Open Session")
                   ("." aidermacs-run-in-current-dir "Start in Current Dir")
                   ("l" aidermacs-clear-chat-history "Clear Chat History")
                   ("s" aidermacs-reset "Reset Session")
                   ("x" aidermacs-exit "Exit Session"))
                  "Files"
                  (("f" aidermacs-add-file "Add File")
                   ("F" aidermacs-add-current-file "Add Current File")
                   ("d" aidermacs-add-same-type-files-under-dir "Add From Directory (same type)")
                   ("w" aidermacs-add-files-in-current-window "Add From Window")
                   ("m" aidermacs-batch-add-dired-marked-files "Add From Dired (marked)")
                   ("j" aidermacs-drop-file "Drop File")
                   ("J" aidermacs-drop-current-file "Drop Current File")
                   ("k" aidermacs-batch-drop-dired-marked-files "Drop From Dired (marked)")
                   ("K" aidermacs-drop-all-files "Drop All Files"))
                  "Code"
                  (("c" aidermacs-direct-change "Code Change")
                   ("e" aidermacs-question-code "Question Code")
                   ("r" aidermacs-architect-this-code "Architect Change"))
                  "Question"
                  (("Q" aidermacs-question-general "General Question")
                   ("p" aidermacs-question-this-symbol "Question This Symbol")
                   ("g" aidermacs-accept-change "Accept Proposed Changes"))
                  "Misc."
                  (("i" aidermacs-implement-todo "Implement TODO")
                   ("t" aidermacs-write-unit-test "Write Test")
                   ("T" aidermacs-fix-failing-test-under-cursor "Fix Test")
                   ("!" aidermacs-debug-exception "Debug Exception"))))

  :init
  (setenv "OPENAI_API_KEY" (password-store-get "aider-openai-key"))
  (setenv "ANTHROPIC_API_KEY" (password-store-get "aider-claude-key"))
  (setenv "DEEPSEEK_API_KEY" (password-store-get "aider-deepseek-key"))
  (setenv "GEMINI_API_KEY" (password-store-get "aider-gemini-key"))

  :config
  (setq aidermacs-backend 'vterm)
  (setq aidermacs-show-diff-after-change nil)
  (setq aidermacs-use-architect-mode nil)
  (setq aidermacs-architect-model "deepseek/deepseek-reasoner")
  (setq aidermacs-default-model "gemini/gemini-2.5-pro-exp-03-25")
  (setq aidermacs-architect-model "r1")
  (setq aidermacs-editor-model "sonnet")
  (setq aidermacs-extra-args '("--cache-prompts" "--no-gitignore")))


;;; Markdown


(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (progn
    (setq markdown-command "marked")
    (add-hook 'markdown-mode-hook #'turn-on-auto-fill)))


;;; PlantUML


(defvar homebrew-plantuml-jar-path
  (car (last (file-expand-wildcards "/usr/local/Cellar/plantuml/*/libexec/plantuml.jar"))))

(use-package plantuml-mode
  :straight t
  :mode ("\\.puml\\'")
  :custom
  (plantuml-default-exec-mode 'jar)
  :config
  (progn
    (if homebrew-plantuml-jar-path
        (setq plantuml-jar-path homebrew-plantuml-jar-path)
      (message "PlantUML jar not found. Preview will not work."))

    (eval-after-load "org-src"
      '(add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

    (eval-after-load "ob-plantuml"
      '(when homebrew-plantuml-jar-path
        (setq org-plantuml-jar-path homebrew-plantuml-jar-path)))))

(use-package flycheck-plantuml
  :straight t
  :after (flycheck)
  :config
  (flycheck-plantuml-setup))


(provide 'jp-prog)
;;; jp-prog.el ends here
