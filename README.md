# What is this

A quick-start [Clojure](https://clojure.org) development environment for use with [Aquamacs](https://aquamacs.org).

# Setup

* Install [Aquamacs](https://aquamacs.org)
* Copy the .el files in this repository to ~/Library/Preferences/Aquamacs\ Emacs/
  * TODO - Expand on instructions here with Github release zip, mkdir, cp.
* Launch Aquamacs

# Using Clojure

To begin with Clojure:

* Launch Aquamacs.app
* Create a new file with the extension `.clj`, `.cljc`, `.cljs` or `.edn`
* Press Control-c then Control-z
* Type your preferred clojure startup command, the default of "clojure" is fine to start.
    * Use up arrow and down arrow keys to browse through prior commands used.
    * `Option-R` to search for an item in history that matches the entered text.
* Your window will split, and a Clojure REPL will appear with `user=>`

Code can be sent from a file to the REPL with [Clojure shortcuts](#clojure), or entered directly in the REPL.

## .dir-locals.el

A file named `.dir-locals.el` can be placed anywhere upstream of the file you run clojure on.

The `.dir-locals.el` nearest to your current file will be loaded.

~~~
((clojure-mode (clj-repl-command "clj -A:test"
				                 "clj -X:socketserver :port 1337"
				                 "java -jar clojure.jar"
				                 "clojure")))
~~~

When running `run-clojure` with this `.dir-locals.el` you will be presented with `clj -A:test` as the command to run.

The other commands will be available for quick access in your immediate command history.

TODO: Coalesce with implicit minibuffer command history.

# Features

* [Clojure Mode](https://github.com/clojure-emacs/clojure-mode)
* Paredit

# Hotkeys

TODO: Replace emacs function name with simple description

## Paredit

| Hotkey        | Function                    |
| ---           | ---                         |
| M-[           | paredit-wrap-square         |
| M-{           | paredit-wrap-curly          |
| M-\<right\>   | forward-sexp                |
| M-\<left\>    | backward-sexp               |
| M-\<up\>      | backward-up-list            |
| M-\<down\>    | down-list                   |
| \<A-return\>  | paredit-newline             |
| M-q           | paredit-reindent-defun      |
| C-\<left\>    | paredit-forward-barf-sexp   |
| C-M-\<right\> | paredit-backward-barf-sexp  |
| C-\<right\>   | paredit-forward-slurp-sexp  |
| C-M-\<left\>  | paredit-backward-slurp-sexp |
| M-S-s         | paredit-split-sexp          |
| M-s           | paredit-splice-sexp         |
| C-k           | paredit-kill                |
| C-S-k         | paredit-kill-backward       |


## Clojure
     
| Hotkey        | Function                         |
| ---           | ---                              |
| C-z           | (run-lisp "clojure")             |
| C-c C-z       | run-clojure-command              |
| C-:           | clojure-toggle-keyword-string    |
| C-c \<space\> | clojure-align                    |
| C-M-x         | lisp-eval-defun                  |
| C-x C-e       | lisp-eval-last-sexp              |
| C-c C-e       | lisp-eval-last-sexp              |
| C-c C-r       | lisp-eval-region                 |
| C-x C-e       | lisp-eval-last-sexp              |
| C-c C-a       | lisp-show-arglist                |
| C-c C-c       | lisp-compile-defun               |
| C-c C-d       | lisp-describe-sym                |
| C-c C-e       | lisp-eval-defun                  |
| C-c C-f       | lisp-show-function-documentation |
| C-c C-k       | lisp-compile-file                |
| C-c C-l       | lisp-load-file                   |
| C-c C-n       | lisp-eval-form-and-next          |
| C-c C-p       | lisp-eval-paragraph              |
| C-c C-r       | lisp-eval-region                 |
| C-c C-v       | lisp-show-variable-documentation |
| C-M-x         | lisp-eval-defun                  |
| C-M-q         | indent-sexp                      |
