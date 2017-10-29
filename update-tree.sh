#!/bin/bash

echo "Updating git subtrees"

git subtree pull --squash --prefix lisp/hydra https://github.com/abo-abo/hydra.git master
git subtree pull --squash --prefix lisp/use-package https://github.com/jwiegley/use-package.git master
git subtree pull --squash --prefix lisp/projectile https://github.com/bbatsov/projectile.git master
git subtree pull --squash --prefix lisp/swiper https://github.com/abo-abo/swiper.git master
git subtree pull --squash --prefix lisp/avy https://github.com/abo-abo/avy.git master
git subtree pull --squash --prefix lisp/counsel-projectile https://github.com/ericdanan/counsel-projectile.git master
git subtree pull --squash --prefix lisp/company https://github.com/company-mode/company-mode master
git subtree pull --squash --prefix lisp/graphene-meta-theme https://github.com/rdallasgray/graphene-meta-theme.git master
git subtree pull --squash --prefix lisp/smartparens https://github.com/Fuco1/smartparens.git master
git subtree pull --squash --prefix lisp/web-mode https://github.com/fxbois/web-mode.git master
git subtree pull --squash --prefix lisp/dash.el https://github.com/magnars/dash.el.git master
git subtree pull --squash --prefix lisp/exec-path-from-shell https://github.com/purcell/exec-path-from-shell.git master
git subtree pull --squash --prefix lisp/monokai-emacs https://github.com/oneKelvinSmith/monokai-emacs.git master
git subtree pull --squash --prefix lisp/expand-region.el https://github.com/magnars/expand-region.el.git master
git subtree pull --squash --prefix lisp/magit https://github.com/magit/magit.git 2.11.0
git subtree pull --squash --prefix lisp/with-editor https://github.com/magit/with-editor v2.7.0
git subtree pull --squash --prefix lisp/aggressive-indent-mode https://github.com/Malabarba/aggressive-indent-mode.git 1.8.3
git subtree pull --squash --prefix lisp/wgrep https://github.com/mhayashi1120/Emacs-wgrep.git 2.1.10
git subtree pull --squash --prefix lisp/smex https://github.com/nonsequitur/smex.git master
git subtree pull --squash --prefix lisp/go-mode https://github.com/dominikh/go-mode.el.git v1.5.0
git subtree pull --squash --prefix lisp/ibuffer-projectile https://github.com/purcell/ibuffer-projectile master
git subtree pull --squash --prefix lisp/emacs-which-key https://github.com/justbur/emacs-which-key.git v3.0.2
git subtree pull --squash --prefix lisp/mwim.el https://github.com/alezost/mwim.el.git master
git subtree pull --squash --prefix lisp/restclient.el https://github.com/pashky/restclient.el.git master
git subtree pull --squash --prefix lisp/iedit https://github.com/victorhge/iedit.git master
git subtree pull --squash --prefix lisp/sbt-mode https://github.com/ensime/emacs-sbt-mode.git v0.3
git subtree pull --squash --prefix lisp/scala-mode https://github.com/ensime/emacs-scala-mode.git v1.0.0
git subtree pull --squash --prefix lisp/ensime https://github.com/ensime/ensime-emacs v2.0.1
git subtree pull --squash --prefix lisp/s https://github.com/magnars/s.el.git 1.12.0
git subtree pull --squash --prefix lisp/popup-el https://github.com/auto-complete/popup-el.git master
git subtree pull --squash --prefix lisp/yasnippet https://github.com/joaotavora/yasnippet master
git subtree pull --squash --prefix lisp/yasnippet-snippets https://github.com/AndreaCrotti/yasnippet-snippets.git master
git subtree pull --squash --prefix lisp/dockerfile-mode https://github.com/spotify/dockerfile-mode.git master
git subtree pull --squash --prefix lisp/yaml-mode https://github.com/yoshiki/yaml-mode.git master
git subtree pull --squash --prefix lisp/ample-theme https://github.com/jordonbiondo/ample-theme.git master
git subtree pull --squash --prefix lisp/zenburn https://github.com/bbatsov/zenburn-emacs master
git subtree pull --squash --prefix lisp/git-timemachine https://github.com/pidu/git-timemachine.git master
git subtree pull --squash --prefix lisp/emacs-memoize https://github.com/skeeto/emacs-memoize.git master
git subtree pull --squash --prefix lisp/font-lock-plus https://github.com/emacsmirror/font-lock-plus.git master
git subtree pull --squash --prefix lisp/all-the-icons.el https://github.com/domtronn/all-the-icons.el.git master
