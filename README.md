# Emacs for me

## Requirements

* Emacs 26 or higher.
* Cask

## Installation

```
git clone --depth=1 git@github.com:223kazuki/emacs.d.git ~/.emacs.d
cd ~/.emacs.d && make install
```

## Shortcuts

* `C-x /`: undo
* `O-left/right`:  backward-word/forward-word
* `C-home/end`: beginning-of-buffer/end-of-buffer
* `O-backspace`: backward-kill-word
* `C-x C-f`: find-file
* `C-x h`: mark-whole-buffer
* `C-z`: suspend -> restore with `fg`
* `M-!`: run command
* `M-g g`: go to line
* `C-f` helm-projectile-fd
* `C-a` helm-projectile-ag
* `C-c o f`: helm-open-github-from-file
* `C-c o c`: helm-open-github-from-commit
* `C-c o i`: helm-open-github-from-issues
* `C-c p p`: helm-open-github-from-pull-requests
* `C-t`: er/expand-region
* `C-n`: highlight-symbol-next
* `C-p`: highlight-symbol-previous
* `C-s`: consult-line
* `M-g`: consult-goto-line
* `C-h`: yas-insert-snippet

### Window

* `C-x 2`: split-window-below
* `C-x 3`: split-window-right
* `C-x 1`: delete-other-windows
* `C-x o`: other-window


### Paredit

* `C-right`: slurp
* `C-left`: barf
* `O-s`: splice
* `O-up`: splice with delete children before cursor
* `O-down`: splice with delete children after cursor
* `O-r`: rise: splice with only last child	

### Clojure mode

* `Ctl-c SPC`: clojure-align

### Cider
https://github.com/clojure-emacs/cider/blob/master/doc/interactive_programming.md

* `C-c O-j`: cider-jack-in-clj
* `C-c O-J`: cider-jack-in-cljs
* `C-x C-e`: cider-eval-last-sexp
* `C-c C-v w`: cider-eval-last-sexp-and-replace
* `C-c O-e`: cider-eval-last-sexp-to-repl
* `C-C O-p`: cider-eval-last-sexp-to-repl
* `C-c C-b`: cider-interrupt
* `C-c C-k`: cider-load-buffer
* `C-c C-u`: cider-undef
* `C-c C-z`: cider-switch-to-repl-buffer
* `O-.`: cider-find-var
* `C-c C-d d`: cider-doc
* `C-c C-s r`: sesman-restart: restart cider session.
* `C-c O-c`: cide connect to clj
* `C-c C-v C-f e`: cider-pprint-eval-last-sexp
* `C-c C-m`: cider-macroexpand-1
* `C-c C-m`: cider-macroexpand-all

### Enlighten mode

`M-x cider-enlighten-mode`

### Debug

put `#break` before target form

### clj-refactor
https://github.com/clojure-emacs/clj-refactor.el/wiki

* `C-c r`: prefix
* `ap`: Add project dependency: hot reload temporarily disabled due to make the middleware run with Java 10.
* `cm`: Clean namespace form
* `dk`: destructure keys
* `il`: Introduce let: `C-g` to disable multi-cursor
* `pf`: Promote function
* `rf`: rename file or dir updating any affected files
* `tf`: wrap in thread-first (->) and fully thread
* `tl`: wrap in thread-last (->>) and fully thread
* `ua`: Unwind all

### dired
https://github.com/jasonm23/emacs-cheat-sheets

* `U`: unmarke all
* `C-x C-f`: new file
