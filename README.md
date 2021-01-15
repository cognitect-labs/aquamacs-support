# What is this

A quick-start [Clojure](https://clojure.org) development environment for use with [Aquamacs](https://aquamacs.org).

# Setup for experienced developers

This setup does not reflect final clojure.org quick-start instructions. It assumes familiarity with git and the command line.

* Install [Aquamacs](https://aquamacs.org/download.html) 3.5 or newer.
* Launch, then quit, Aquamacs.app to create the preferences directory **OR**
  * `mkdir ~/Library/Preferences/Aquamacs\ Emacs/` in your terminal
* Clone this repository to your preferred location.
* cd to the repository
* `cp preferences.el ~/Library/Preferences/Aquamacs Emacs/Preferences.el`
* Launch Aquamacs.app

Information presented about "Compiling" and "Warnings" on the first run are normal.

# Setup for new Clojure enthusiasts

These instructions assume a downloadable `Preferences.el` on clojure.org, and that the user has Clojure.

* Install [Aquamacs](https://aquamacs.org/download.html) 3.5 or newer.
* Launch, then quit, Aquamacs.app to create the preferences directory.
* Download the setup package (link here)
* Copy `preferences.el` to `~/Library/Preferences/Aquamacs Emacs/`
* Launch Aquamacs.app

Information presented about "Compiling" and "Warnings" on the first run are normal. Click that window and close it with `command-w`

# Using Clojure

To begin with Clojure:

* Launch Aquamacs.app
* Create a new file with the extension `.clj`
* Press Control-c then Control-z
* Type your preferred clojure startup command, the default of "clojure" is fine to start.
* Your window will split, and a Clojure REPL will appear with `user=>`

# Features

* [Clojure Mode](https://github.com/clojure-emacs/clojure-mode)
* Paredit

# Hotkeys

TODO: Replace emacs function name with simple description

## Paredit

Hotkey | Function
| --- | --- |
| M-[ 	| paredit-wrap-square |
| M-{ 	| paredit-wrap-curly |
| M-<right> 	| forward-sexp |
| M-<left> 	| backward-sexp |
| M-<up> 	| backward-up-list |
| M-<down> 	| down-list |
| <A-return> 	| paredit-newline |

## Clojure
     
Hotkey | Function
| --- | --- |
| C-: | clojure-toggle-keyword-string |
| C-c <space> | clojure-align |
| C-M-x 	| lisp-eval-defun |
| C-x C-e | lisp-eval-last-sexp |
| C-c C-e | lisp-eval-last-sexp |
| C-c C-z | run-clojure |
| C-c C-r | lisp-eval-region |
| C-x C-e | lisp-eval-last-sexp |
| C-c C-a | lisp-show-arglist |
| C-c C-c | lisp-compile-defun |
| C-c C-d | lisp-describe-sym |
| C-c C-e | lisp-eval-defun |
| C-c C-f | lisp-show-function-documentation |
| C-c C-k | lisp-compile-file |
| C-c C-l | lisp-load-file |
| C-c C-n | lisp-eval-form-and-next |
| C-c C-p | lisp-eval-paragraph |
| C-c C-r | lisp-eval-region |
| C-c C-v | lisp-show-variable-documentation |
| C-c C-z | run-clojure |
| C-M-x 	| lisp-eval-defun |
| C-M-q 	| indent-sexp |
