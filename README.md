# What is this

A quick-start [Clojure](https://clojure.org) development environment for use with [Aquamacs](https://aquamacs.org).

# Setup

* Install [Aquamacs](https://aquamacs.org)
* Copy the .el files in this repository to ~/Library/Preferences/Aquamacs\ Emacs/
  * TODO - How to create prefs directory? Launch/Close Aquamacs or mkdir?
  * TODO - Expand on instructions here with Github release zip, mkdir, cp.
* Launch Aquamacs

# Using Clojure

To begin with Clojure:

* Launch Aquamacs.app
* Create a new file, or open a file, with the extension `.clj`, `.cljc`, `.cljs` or `.edn`
* Press Control-z
  * To use a specific clojure command, type Control-c then Control-z
    * Use up arrow and down arrow keys to browse through prior commands used.
    * `Option-R` to search for an item in history that matches the entered text.
* Your window will split, and a Clojure REPL will appear with `user=>`

Code can be sent from a file to the REPL with [Clojure shortcuts](#clojure), or entered directly in the REPL.

## .dir-locals.el

A file named `.dir-locals.el` can be placed anywhere upstream of the file you run clojure on.

~~~
((clojure-mode (clj-repl-command "clj -A:test"
				                 "clj -X:socketserver :port 1337"
				                 "java -jar clojure.jar"
				                 "clojure")))
~~~

When running `run-clojure-command` with this `.dir-locals.el` you will be able to:

* Press enter to immediately use the first option `clj -A:test`
* Browse other options with up and down arrow keys.
  * All previously used REPL launching commands are available as options after browsing past the last item in your .dir-locals.el
* Search for a command in your .dir-locals.el and command history with `Option-r`

## .dir-locals.el Location

If you `C-c C-z` on `~/my-project/src/killer-app.clj` then the command will look for settings in the following order, and pick the first one found:

* `~/my-project/src/.dir-locals.el`
* `~/my-project/.dir-locals.el`
* `~/.dir-locals.el`
* `/Users/.dir-locals.el`
* `/.dir-locals.el`

It is recommended to keep a single `.dir-locals.el` in the root directory of your project.

If the project already has a `.dir-locals.el` that you can not change (perhaps it's under version control), then you can place your user settings in `.dir-locals-2.el`.

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

# Package Sources

* [clojure-mode 5.12.0](https://github.com/clojure-emacs/clojure-mode/releases/tag/5.12.0)
* [paredit stable](http://mumble.net/~campbell/git/paredit.git/)
