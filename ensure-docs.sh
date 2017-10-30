#!/bin/bash

# An one-off script for adding header/footer to my elisp files.

find config -name "*.el" | while read f; do
    fname=$(basename "$f")
    if ! grep -q $fname "$f"; then
        echo "Adding header/footer to $f"
        tmpfile=$(mktemp)
        cat > $tmpfile <<EOF
;;; $fname --- An awesome elisp package -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

EOF
        cat "$f" >> $tmpfile
        echo ";;; $fname ends here" >> $tmpfile
        cp $tmpfile $f
        rm $tmpfile
    fi
done
