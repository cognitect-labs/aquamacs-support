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
     (define-key paredit-mode-map (kbd "M-r") 'paredit-raise-sexp)
     (define-key paredit-mode-map (kbd "C-k") 'paredit-kill)
     (define-key paredit-mode-map (kbd "C-S-k") 'paredit-kill-backward) ;; Does not exist! 
     (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
     (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
     (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
     (define-key paredit-mode-map (kbd "M-<right>") 'forward-sexp)
     (define-key paredit-mode-map (kbd "M-<left>") 'backward-sexp)
     (define-key paredit-mode-map (kbd "M-<up>") 'backward-up-list)
     (define-key paredit-mode-map (kbd "M-<down>") 'down-list)
     (define-key paredit-mode-map (kbd "<A-return>") 'paredit-newline)))

(eval-after-load 'clojure-mode
  '(progn
     (define-key paredit-mode-map (kbd "C-z") 'run-clojure-no-prompt)
     (define-key paredit-mode-map (kbd "C-c C-z") 'run-clojure)
     (define-key paredit-mode-map (kbd "C-M-x") 'lisp-eval-defun) ;; primary eval command
     (define-key paredit-mode-map (kbd "C-c C-e") 'lisp-eval-defun)
     (define-key paredit-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
     (define-key paredit-mode-map (kbd "C-c C-l") 'lisp-load-buffer)
     (define-key paredit-mode-map (kbd "C-c C-n") 'lisp-eval-form-and-next)
     (define-key paredit-mode-map (kbd "C-c C-p") 'lisp-eval-paragraph)
     (define-key paredit-mode-map (kbd "C-c C-r") 'lisp-eval-region)
     (define-key paredit-mode-map (kbd "C-M-q") 'indent-sexp)
     (define-key paredit-mode-map (kbd "C-c C-v") 'lisp-show-variable-documentation) ;; not working currently
     (define-key paredit-mode-map (kbd "C-c C-a") 'lisp-show-arglist) ;; not working currently
     (define-key paredit-mode-map (kbd "C-c C-c") 'lisp-compile-defun) ;; not working currently
     (define-key paredit-mode-map (kbd "C-c C-d") 'lisp-describe-sym) ;; not working currently
     (define-key paredit-mode-map (kbd "C-c C-f") 'lisp-show-function-documentation) ;; not working currently
     (define-key paredit-mode-map (kbd "C-c C-k") 'lisp-compile-file) ;; not working currently
     ))

(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook              #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook  #'enable-paredit-mode)
(add-hook 'clojure-mode-hook           #'enable-paredit-mode)

(defun lisp-load-buffer ()
  "Start at top of buffer, eval each form in the inferior-lisp buffer until reaching end of file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (lisp-eval-form-and-next))))

(setq inferior-lisp-program "clojure")

(defvar clj-repl-command)

(defvar clj-repl-command-history '())

(add-to-list 'savehist-additional-variables 'clj-repl-command-history)

(defun run-clojure-no-prompt ()
  (interactive)
  (if (and (boundp 'clj-repl-command)
           (stringp (car clj-repl-command)))
      (run-clojure-command (car clj-repl-command))
    (run-clojure-command "clojure")))

(defun run-clojure (cmd)
  (interactive (list
                (if (boundp 'clj-repl-command)
                    (let ((first-command (car clj-repl-command))
                          (rest-commands (if clj-repl-command-history
						                     (append (cdr clj-repl-command) clj-repl-command-history)
						                   (cdr clj-repl-command))))
                      (read-from-minibuffer "Command:" first-command nil nil 'rest-commands))
                  (read-from-minibuffer "Command:" "clojure" nil nil 'clj-repl-command-history))))
  (run-clojure-command cmd))

(defun run-clojure-command (cmd)
  (let ((dd (if (and (fboundp 'clojure-project-root-path)
                     (stringp (clojure-project-root-path)))
			    (clojure-project-root-path)
			  default-directory))
	    cb (curent-buffer))
    (cd dd)
    (add-to-list 'clj-repl-command-history cmd)
    (if (boundp 'lisp-environment)
        (let ((process-environment (append process-environment clj-environment)))
          (run-lisp cmd))
      (run-lisp cmd))
    (switch-to-buffer cb)
    (switch-to-buffer-other-window "*inferior-lisp*")))

;; Remove this line to disable warnings about unsafe variables when using .dir-locals with 'run-command
;; Only use this if you are certain of the integrity of .dir-locals files upstream of where you launch your REPL
;; (put 'clj-repl-command 'safe-local-variable (lambda () t))
