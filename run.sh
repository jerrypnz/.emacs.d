#!/bin/bash

if [ -x /Applications/Emacs.app/Contents/MacOS/Emacs ]; 
then
    EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
elif [ -x /Applications/Aquamacs.app/Contents/MacOS/Aquamacs ];
then
    EMACS=/Applications/Aquamacs.app/Contents/MacOS/Aquamacs
else
    EMACS=emacs
fi

$EMACS -batch -no-site-file -Q --eval '(setq standard-indent 2)' --eval '(setq-default indent-tabs-mode nil)' --eval '(load-file "web-mode.el")' --eval '(web-mode)' -f web-mode-test

