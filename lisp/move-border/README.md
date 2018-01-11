move-border
===========

Move-border provides functions for resizing Emacs windows, considering the
current window border instead of the window itself.

So, by binding your keys to `move-border-up`, `move-border-down`,
`move-border-left`, and `move-border-right`, you can resize your windows much
more intuitively than if you were using `shrink-window`, `enlarge-window`,
`shrink-window-horizontally` and `enlarge-window-horizontally`.

```lisp
;; example key bindings
(global-set-key (kbd "M-S-<up>") 'move-border-up)
(global-set-key (kbd "M-S-<down>") 'move-border-down)
(global-set-key (kbd "M-S-<left>") 'move-border-left)
(global-set-key (kbd "M-S-<right>") 'move-border-right)
```


Disclaimer
----------

I am not the author of this file, but found it on a blog where the blog author
is also saying that he is not the author. So, no one knows who is the author.

If you are the author, please send me a word (contact@ramnes.eu).


License
-------

(c) the unknown original author
