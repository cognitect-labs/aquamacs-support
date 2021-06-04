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

;; Keybindings

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
     (define-key paredit-mode-map (kbd "C-M-x") 'clj-eval-defun) ;; primary eval command
     (define-key paredit-mode-map (kbd "C-c C-e") 'clj-eval-defun)
     (define-key paredit-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
     (define-key paredit-mode-map (kbd "C-c C-l") 'clj-load-file)
     (define-key paredit-mode-map (kbd "C-c C-b") 'clj-load-buffer)
     (define-key paredit-mode-map (kbd "C-c C-n") 'lisp-eval-form-and-next)
     (define-key paredit-mode-map (kbd "C-c C-p") 'lisp-eval-paragraph)
     (define-key paredit-mode-map (kbd "C-c C-r") 'lisp-eval-region)
     (define-key paredit-mode-map (kbd "C-M-q") 'indent-sexp)
     (define-key paredit-mode-map (kbd "C-c C-v") 'lisp-show-variable-documentation)
     (define-key paredit-mode-map (kbd "C-c C-f") 'lisp-show-function-documentation)
     (define-key paredit-mode-map (kbd "C-c C-a") 'lisp-show-arglist)
     (define-key paredit-mode-map (kbd "C-c C-c") 'lisp-compile-defun) ;; not working currently
     (define-key paredit-mode-map (kbd "C-c C-k") 'lisp-compile-file) ;; not working currently
     ))

;; Hooks

(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook              #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook  #'enable-paredit-mode)
(add-hook 'clojure-mode-hook           #'enable-paredit-mode)

;; Vars

(setq lisp-function-doc-command
      "(clojure.repl/doc %s)\n")

(setq lisp-var-doc-command
      "(clojure.repl/doc %s)\n")

(setq lisp-arglist-command
      "(str \"%1$s args: \"
            (or (some-> '%1$s resolve meta :arglists)
                \"Not Found\"))\n")

(setq inferior-lisp-program "clojure")

(setq clojure-build-tool-files '("deps.edn"))

(defvar clj-repl-command)

(defvar clj-repl-command-history '())

(add-to-list 'savehist-additional-variables 'clj-repl-command-history)

;; Clojure Functions

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

(defun set-environment (env-alist)
  (mapcar (lambda (e)
            (let ((key (car e))
                  (val (cdr e)))
              (setenv key val)))
          env-alist))

(defun run-clojure-command (cmd)
  (let* ((dd (if (and (fboundp 'clojure-project-root-path)
		      (stringp (clojure-project-root-path)))
		 (clojure-project-root-path)
	       (let ((dir-locals-dir (car (dir-locals-find-file (buffer-file-name)))))
		 (if dir-locals-dir
		     dir-locals-dir
		   default-directory))))
	 (cb (current-buffer))
	 (cmd-exists (executable-find (car (split-string cmd "\s"))))
	 (cmd-to-run (if cmd-exists
			 cmd
		       (concat dd cmd))))
    (add-to-list 'clj-repl-command-history cmd)
    (if (boundp 'process-env)
        (set-environment process-env))
    (run-lisp cmd-to-run)
    (switch-to-buffer cb)
    (switch-to-buffer-other-window "*inferior-lisp*")))

;; Lisp workflow modifications

;; regex, not plain string
;; TODO allow define in dir-local
(defconst ignored-forms '("comment"))

(defun check-ignored-forms (forms)
  (interactive "P")
  (let ((ret nil))
    (dolist (form ignored-forms)
      (when (looking-at (concat "(" form))
        (setq ret 't)))
    ret))

(defun clj-do-defun (do-region)
  "Send the current defun to the inferior Lisp process.
The actually processing is done by `do-region'. Ignores (comment) forms."
  (save-excursion
    ;; if there's a form after the cursor, jump into it before parsing.
    ;; lisp-eval-defun doesn't do this. Unsure if we should?
    ;;(when (looking-at "(")
    ;;  (down-list))
    (let ((err nil)
          (forms '()))
      ;; build a list of sexp start locations before the cursor position
      ;; error on top-level form and continue
      (while (not (eq err 1))
        (condition-case nil
            (backward-up-list nil t)
          (error (setq err 1)))
        (add-to-list 'forms (point)))
      ;; We're at the top-level defun now, check for comment
      ;; This could check against a list of forms to ignore and
      ;; climb up to the first not-ignored form
      (if (check-ignored-forms ignored-forms) ;;(looking-at "(comment")
          (if (or (eq 1 (length forms)) (null forms))
              (message "No top level form, or top level form ignored.")
            (progn
              (goto-char (cadr forms))
              (forward-sexp)
              (funcall do-region (cadr forms) (point))))
        (progn
         (let ((start (point)))
           (forward-sexp)
           (funcall do-region start (point))))))))

(defun clj-eval-defun (&optional and-go)
  "Send the current defun to the inferior Lisp process, assuming a Clojure REPL."
  (interactive "P")
  (clj-do-defun 'lisp-eval-region)
  (if and-go (switch-to-lisp t)))

(defvar clj-load-file-history '())

(defun clj-load-file (file-name)
  "Send (load-file) to the inferor-lisp process with an argument
of the file associated with the current buffer. Supports history."
  (interactive (list
                (read-from-minibuffer "File Path:" (buffer-file-name) nil nil 'clj-load-file-history)))
  (let ((proc (inferior-lisp-proc))
         (load-string (format "(load-file \"%s\")\n" file-name)))
    (comint-check-source file-name)
    (comint-send-string proc load-string)))

(defun clj-load-buffer ()
  "Start at top of buffer, eval each form in the inferior-lisp buffer until reaching end of file.
This strategy avoids a comint string-length limit on macOS that exists at time of writing."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (lisp-eval-form-and-next))))

;; Optional Settings

;; Remove comment in below line to disable warnings about unsafe variables when using .dir-locals with 'run-command
;; Only use this if you are certain of the integrity of .dir-locals files upstream of where you launch your REPL
;; ----
;; (put 'clj-repl-command 'safe-local-variable (lambda (_) t))

;; Remove comment in below line to automatically enable clojure-mode for files ending in the supplied file extensions.
;; The string arguments are emacs regex. https://www.gnu.org/software/emacs/manual/html_node/emacs/Regexps.html
;; ----
;; (add-to-list 'auto-mode-alist '("\\.repl$\\'" . clojure-mode))
