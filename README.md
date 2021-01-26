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
(clojure-mode (clj-repl-command "clojure -A:dev"
                                "clojure -A:test"))
~~~

When using `Control-c Control-z` with this `.dir-locals.el` you will be able to:

* Press enter to immediately use the first option `clj -A:test`
* Browse other options with up and down arrow keys.
  * All previously used REPL launching commands are available as options after browsing past your `.dir-locals.el` provided commands.
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

# What is Paredit?

Clojure code is data, and it can be edited and navigated with a concept called "Structured Editing". A package named "Paredit" for structured editing.

Paredit allows you to edit and navigate Clojure code in a regular and predictable manner by acting on forms.

Code:

`(println "1 + 2 =" (+ 1 2))`

Forms:

* `(println "1 + 2 =" (+ 1 2))`
  * `println`
  * `"1 + 2 = "`
  * `(+ 1 2)`
    * `+`
    * `1`
    * `2`
  
Each data structure is a form, and each atomic unit is a form. This relationship allows for editing and navigation that acts on the structure of the code. Structured editing differs from editing systems that need to understand what the code _means_ to act in a logical manner.

Paredit expects all of your delimiters (`()`, `[]` `{}`) to be in matching pairs. With very few exceptions, if you use Paredit for your code editing and restructuring, then your code will remain structured. 

## Using Paredit

Paredit's commands offer 3 types of operation: creating structure, navigating structure and changing the structure of code.

The examples indicate the cursor with a `❚`.

**TODO** - These are best presented as _slow_ and deliberate animations. Current sources utilize fast-moving and confusing code screen-caps.

### Create Structure

Creating structure is simple as inserting a delimiter like `()`. There are two ways to do this.

Insert a new delimiter with `(`, `[`, `{` or `"`

* Ex 1.
  *  `❚`
  * `(❚)`

Wrap the next form with a delimiter using `Option-(`, `Option-[` or `

* Ex 1.
  * `(println "1 + 2 =" ❚(+ 1 2))`
  * `(println "1 + 2 =" (❚(+ 1 2)))`

### Navigation

The navigation commands can be accompanied by the `Shift` key to make a selection from the starting cursor location to the ending cursor location. Using `Shift` is most common for selecting forms with `Option-Shift-right` and `Option-Shift-left`.

Paredit has many quick shortcuts, but they can be daunting to remember while you are learning. __Most structural editing can be accomplished by selecting a form, then using your clipboard and cursor movement to do something with it__.

Move forward over forms with `Option-right`.

* Ex 1.
  * `❚(println "1 + 2 =" (+ 1 2))`
  * `(println "1 + 2 =" (+ 1 2))❚`

* Ex 2.
  * `(❚println "1 + 2 =" (+ 1 2))`
  * `(println❚ "1 + 2 =" (+ 1 2))`

* Ex 3.
  * `(println "1 + 2 =" ❚(+ 1 2))`
  * `(println "1 + 2 =" (+ 1 2)❚)`

Move backward over forms with `Option-left`.

* Ex 1.
  * `(println "1 + 2 =" (+ 1 2))❚`
  * `❚(println "1 + 2 =" (+ 1 2))`

* Ex 2.
  * `(println "1 + 2 ="❚ (+ 1 2))`
  * `(println ❚"1 + 2 =" (+ 1 2))` 

* Ex 3.
  * `(println "1 + 2 =" (+ 1 2)❚)`
  * `(println "1 + 2 =" ❚(+ 1 2))` 

Move into the next form of forms (??) with `Option-down`

* Ex 1.
  * `❚(println "1 + 2 =" (+ 1 2))`
  * `(❚println "1 + 2 =" (+ 1 2))`

* Ex 2.
  * `(❚println "1 + 2 =" (+ 1 2))`
  * `(println "1 + 2 =" (❚+ 1 2))`

Move out of the current containing form (??) with `Option-up`

* Ex 1.
  * `(println "1 + 2 =" (+ 1❚ 2))`
  * `(println "1 + 2 =" ❚(+ 1 2))`

* Ex 2.
  * `(println "1 + 2 =" ❚(+ 1 2))`
  * `❚(println "1 + 2 =" (+ 1 2))`

Move after the pressed delimiter to the right. Inside a double quote, only `"` works, and only when the cursor is next to a double quote.

* Ex 1. `)`
  * `(println "1 + 2 =" (+ 1❚ 2))`
  * `(println "1 + 2 =" (+ 1 2)❚)`

* Ex 2. `]`
  * `(let [k ❚(+ 2 2)])`
  * `(let [k (+ 2 2)]❚)`

* Ex 3. `"`
  * `(println "1 + 2 =❚" (+ 1 2))`
  * `(println "1 + 2 ="❚ (+ 1 2))`

* Ex 4. `"`
  * `(println "1 +❚ 2 =" (+ 1 2))`
  * `(println "1 +\"❚ 2 =" (+ 1 2))`

### Edit Structure

It is tempting to try to delete single delimiters. Deleting a single delimiter will unbalance your code structure. The desire to delete a delimiter indicates that you are trying to change your code structure, and Paredit offers a number of functions that are more efficient and safer than attempting to manually move delimiters.

All of these functions can be replicated by using `shift-option-right` or `shift-option-left` to select the next or previous form. Once a form is selected you can delete it and create new structure, or you can cut it and paste it elsewhere.

`Control` with left and right arrow keys controls the rightmost outer delimiter of the current surrounding form. 

`Control-Option` with left and right arrow keys controls the leftmost outer delimiter of the current surrounding form. 

Bring the next form into the current form with `Control-right`. This moves the outermost surrounded delimiter _on the right_, to the right.

* Ex 1. Adding the form to the right to the current form
  * `(str "Hello"❚) "World"`
  * `(str "Hello"❚ "World)`

Move the next form out of the current form with `Control-left`. This moves the outermost surrounded delimiter _on the right_, to the left.

* Ex 1. Move the following form out of the current form.
  * `(when (enthusiastic-greet?) (println "Hello!")❚ (println "How are you?"))`
  * `(when (enthusiastic-greet?) (println "Hello!")❚) (println "How are you?")`
  
`Control-option` with left and right arrow keys controls the rightmost outer delimiter of the current surrounding form. 

Bring the previous form into the current form with `Control-option-right`. This moves the outermost surrounded delimiter _on the left_, to the left.

* Ex 1. Push the form to the left out of the current form. (Rarely used)
  * `(defn simple-vector [] ["Return simple vector" ❚1 2 3])`
  * `(defn simple-vector [] "Return simple vector" [❚1 2 3])`

Move the previous form out of the current form with `Control-option-left`. This moves the outermost surrounded delimiter _on the left_, to the right.

* Ex 1. Add the form on the left to the current form. (Rarely used)
  * `(str (this) "❚is a form")`
  * `(str "(this) ❚is a form")`
  
Split the current form with `Option-Shift-s`.

* Ex 1. Turn one data structure into two.
  * `(map + [1 2❚ 3 4])`
  * `(map + [1 2]❚ [3 4])`
  
 Remove the nearest surrounding `(` and `)` with `Option-s`

* Ex 1. 
  * `[1 2 [3❚ 4]]`
  * `[1 2 3❚ 4]`

  
Reindent the outermost form with `Alt-q`

* Ex 1.
  * ~~~
    (defn calculate-tax [state item-cost]
    (let [rate (case state
    :Florida 0.06
    :Wyoming 0.04
             ❚0.0)]
       (* rate item-cost)))
  * ~~~
    (defn calculate-tax [state item-cost]
      (let [rate (case state
                   :Florida 0.06
                   :Wyoming 0.04
                   ❚0.0)]
        (* rate item-cost)))
        
#### Deleting

Paredit tries to prevent you from unbalancing your forms. This can lead to situations where it appears that your backspace or delete key does not work. Using backspace over a right parenthesis will move the cursor instead of deleting something.

Think about editing code in terms of adding, moving and deleting forms rather than adding, moving and deleting characters. When in doubt use `Option-Shift-left` and `Option-Shift-right` to select a form, then the `Delete` key (not backspace!) to delete it.

Delete everything to the right of the cursor without unbalancing the current form with `Control-k`

* Ex 1.
  * `(println "1 + 2 =" ❚(+ 1 2))`
  * `(println "1 + 2 =" ❚)`

* Ex 2.
  * `(❚println "1 + 2 =" (+ 1 2))`
  * `(❚)`
  
* Ex 3.
  * `❚(println "1 + 2 =" (+ 1 2))`
  * `❚`

# Features

* [Clojure Mode](https://github.com/clojure-emacs/clojure-mode)
* Paredit

# Hotkeys

TODO: Replace emacs function name with simple description

## Paredit

| Hotkey                   | Function                    |
| ---                      | ---                         |
| Option-\<right\>         | forward-sexp                |
| Option-\<left\>          | backward-sexp               |
| Option-\<up\>            | backward-up-list            |
| Option-\<down\>          | down-list                   |
| Option-[                 | paredit-wrap-square         |
| Option-{                 | paredit-wrap-curly          |
| Option-(                 | paredit-wrap-round          |
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
| Control-z           | Start REPL with first .dir-locals value, or clojure" |
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
