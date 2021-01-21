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
((clojure-mode (clj-repl-command "clojure -A:dev"
    "clojure -X:socketserver :port 1337"
				                 "java -jar clojure.jar"
				                 "clojure")))
~~~

When using `Control-c Control-z` with this `.dir-locals.el` you will be able to:

* Press enter to immediately use the first option `clj -A:test`
* Browse other options with up and down arrow keys.
  * All previously used REPL launching commands are available as options after browsing past the last item in your .dir-locals.el
* Search for a command in your .dir-locals.el and command history with `Option-r`

`Control-z` will automatically launch a REPL with the first defined command in the loaded `.dir-locals.el`

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

| Hotkey                   | Function                    |
| ---                      | ---                         |
| Option-[                 | paredit-wrap-square         |
| Option-{                 | paredit-wrap-curly          |
| Option-(                 | paredit-wrap-round          |
| Option-\<right\>         | forward-sexp                |
| Option-\<left\>          | backward-sexp               |
| Option-\<up\>            | backward-up-list            |
| Option-\<down\>          | down-list                   |
| \<A-return\>             | paredit-newline             |
| Option-q                 | paredit-reindent-defun      |
| Control-\<left\>         | paredit-forward-barf-sexp   |
| Control-Option-\<right\> | paredit-backward-barf-sexp  |
| Control-\<right\>        | paredit-forward-slurp-sexp  |
| Control-Option-\<left\>  | paredit-backward-slurp-sexp |
| Option-Shift-s           | paredit-split-sexp          |
| Option-s                 | paredit-splice-sexp         |
| Control-k                | paredit-kill                |
| Control-Shift-k          | paredit-kill-backward       |


## Clojure
     
| Hotkey              | Function                                              |
| ---                 | ---                                                   |
| Control-z           | Start REPL with first .dir-locals value, or "clojure" |
| Control-c Control-z | Start REPL, prompt for command                        |
| Control-:           | clojure-toggle-keyword-string                         |
| Control-c \<space\> | clojure-align                                         |
| Control-Option-x    | lisp-eval-defun                                       |
| Control-c Control-e | lisp-eval-defun                                       |
| Control-x Control-e | lisp-eval-last-sexp                                   |
| Control-c Control-e | lisp-eval-last-sexp                                   |
| Control-c Control-r | lisp-eval-region                                      |
| Control-Option-q    | indent-sexp                                           |


### Currently not operational or unexplored

Additional code needed to enable:

| Control-c Control-a | lisp-show-arglist                                     |
| Control-c Control-c | lisp-compile-defun                                    |
| Control-c Control-d | lisp-describe-sym                                     |
| Control-c Control-f | lisp-show-function-documentation                      |
| Control-c Control-v | lisp-show-variable-documentation                      |

Unexplored:

| Control-c Control-p | lisp-eval-paragraph                                   |
| Control-c Control-k | lisp-compile-file                                     |
| Control-c Control-l | lisp-load-file                                        |
| Control-c Control-n | lisp-eval-form-and-next                               |

# Package Sources

* [clojure-mode 5.12.0](https://github.com/clojure-emacs/clojure-mode/releases/tag/5.12.0)
* [paredit stable](http://mumble.net/~campbell/git/paredit.git/)
