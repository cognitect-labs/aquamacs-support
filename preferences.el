;; Copyright 2021 Cognitect Inc.
;; 
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;; 
;; http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.


;;TODO - Temporarily disable until certain it's necessary for workflows
;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))

(require 'clojure-mode)
(require 'paredit)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-q") 'paredit-reindent-defun)
     (define-key paredit-mode-map (kbd "C-<left>") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-M-<right>") 'paredit-backward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-<right>") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-M-<left>") 'paredit-backward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-S-s") 'paredit-split-sexp)
     (define-key paredit-mode-map (kbd "M-s") 'paredit-splice-sexp)
     (define-key paredit-mode-map (kbd "C-k") 'paredit-kill)
     (define-key paredit-mode-map (kbd "C-S-k") 'paredit-kill-backward)
     (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
     (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
     (define-key paredit-mode-map (kbd "M-<right>") 'forward-sexp)
     (define-key paredit-mode-map (kbd "M-<left>") 'backward-sexp)
     (define-key paredit-mode-map (kbd "M-<up>") 'backward-up-list)
     (define-key paredit-mode-map (kbd "M-<down>") 'down-list)
     (define-key paredit-mode-map (kbd "<A-return>") 'paredit-newline)))

(eval-after-load 'clojure-mode
  '(progn
     (define-key paredit-mode-map (kbd "C-c C-z") 'run-clojure-cmd)
     (define-key paredit-mode-map (kbd "C-z") '(lambda () (run-lisp "clojure")))
     (define-key paredit-mode-map (kbd "C-M-x") 'lisp-eval-defun) ;; primary eval mode
     (define-key paredit-mode-map (kbd "C-c C-e") 'lisp-eval-defun)
     (define-key paredit-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
     (define-key paredit-mode-map (kbd "C-c C-r") 'lisp-eval-region)
     (define-key paredit-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
     (define-key paredit-mode-map (kbd "C-c C-l") 'lisp-load-file)
     (define-key paredit-mode-map (kbd "C-c C-n") 'lisp-eval-form-and-next)
     (define-key paredit-mode-map (kbd "C-c C-p") 'lisp-eval-paragraph)
     (define-key paredit-mode-map (kbd "C-c C-r") 'lisp-eval-region)
     (define-key paredit-mode-map (kbd "C-c C-v") 'lisp-show-variable-documentation) ;; not working currently
     (define-key paredit-mode-map (kbd "C-c C-a") 'lisp-show-arglist) ;; not working currently
     (define-key paredit-mode-map (kbd "C-c C-c") 'lisp-compile-defun) ;; not working currently
     (define-key paredit-mode-map (kbd "C-c C-d") 'lisp-describe-sym) ;; not working currently
     (define-key paredit-mode-map (kbd "C-c C-f") 'lisp-show-function-documentation) ;; not working currently
     (define-key paredit-mode-map (kbd "C-c C-k") 'lisp-compile-file) ;; not working currently
     (define-key paredit-mode-map (kbd "C-M-q") 'indent-sexp)))

(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook              #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook  #'enable-paredit-mode)
(add-hook 'clojure-mode-hook           #'enable-paredit-mode)

(setq inferior-lisp-program "clojure")

(defvar clj-repl-command)

(defun run-clojure-command (cmd)
  (interactive (list (if (boundp 'clj-repl-command)
                         (let ((first-command (car clj-repl-command))
                               (rest-commands (cdr clj-repl-command)))
                           (read-from-minibuffer "Command:" first-command nil nil 'rest-commands))
                       (read-from-minibuffer "Command:" "clojure"))))
  (let ((dd (if (and (fboundp 'clojure-project-root-path)
                     (stringp (clojure-project-root-path)))
			    (clojure-project-root-path)
			  default-directory))
	    cb (curent-buffer))
    (cd dd)
    (run-lisp cmd)
    (switch-to-buffer cb)
    (switch-to-buffer-other-window "*inferior-lisp*")))

;; Remove this line to disable warnings about unsafe variables when using .dir-locals with 'run-command
;; Only use this if you are certain of the integrity of .dir-locals files upstream of where you launch your REPL

;; (put 'clj-repl-command 'safe-local-variable (lambda () t))
