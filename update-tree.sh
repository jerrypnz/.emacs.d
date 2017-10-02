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
git subtree pull --squash --prefix lisp/graphene https://github.com/rdallasgray/graphene.git master
git subtree pull --squash --prefix lisp/smartparens https://github.com/Fuco1/smartparens.git master
git subtree pull --squash --prefix lisp/web-mode https://github.com/fxbois/web-mode.git master
git subtree pull --squash --prefix lisp/dash.el https://github.com/magnars/dash.el.git master
git subtree pull --squash --prefix lisp/sr-speedbar https://github.com/emacsorphanage/sr-speedbar.git master
git subtree pull --squash --prefix lisp/exec-path-from-shell https://github.com/purcell/exec-path-from-shell.git master
git subtree pull --squash --prefix lisp/monokai-emacs https://github.com/oneKelvinSmith/monokai-emacs.git master
