;; These options are insecure, but currently necessary for package download in Aquamacs.
;; Disable package signature checking and use TLS 1.2 or lower.
;; Use at your own risk.
(setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(use-package paredit
             :ensure t)
(use-package clojure-mode
             :ensure t)
(use-package inf-lisp
             :ensure t)

(use-package exec-path-from-shell
  :ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq inferior-lisp-program "clojure")

(defun run-clojure (cmd)
  (interactive (list (read-from-minibuffer "Command:" "clojure")))
  (let ((default-directory (if (boundp 'clojure-project-root-path)
			                   (clojure-project-root-path)
			                 default-directory))
	    cb (curent-buffer))
    (run-lisp cmd)
    (switch-to-buffer cb)
    (switch-to-buffer-other-window "*inferior-lisp*")))

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
     (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
     (define-key paredit-mode-map (kbd "M-<right>") 'forward-sexp)
     (define-key paredit-mode-map (kbd "M-<left>") 'backward-sexp)
     (define-key paredit-mode-map (kbd "M-<up>") 'backward-up-list)
     (define-key paredit-mode-map (kbd "M-<down>") 'down-list)
     (define-key paredit-mode-map (kbd "<A-return>") 'paredit-newline)))

(defun add-clojure-eval ()
  (define-key clojure-mode-map (kbd "C-M-x") 'lisp-eval-defun)
  (define-key clojure-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
  (define-key clojure-mode-map (kbd "C-c C-e") 'lisp-eval-last-sexp)
  (define-key clojure-mode-map (kbd "C-c C-z") 'run-clojure)
  (define-key clojure-mode-map (kbd "C-c C-r") 'lisp-eval-region)
  (define-key clojure-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
  (define-key clojure-mode-map (kbd "C-c C-a") 'lisp-show-arglist)
  (define-key clojure-mode-map (kbd "C-c C-c") 'lisp-compile-defun)
  (define-key clojure-mode-map (kbd "C-c C-d") 'lisp-describe-sym)
  (define-key clojure-mode-map (kbd "C-c C-e") 'lisp-eval-defun)
  (define-key clojure-mode-map (kbd "C-c C-f") 'lisp-show-function-documentation)
  (define-key clojure-mode-map (kbd "C-c C-k") 'lisp-compile-file)
  (define-key clojure-mode-map (kbd "C-c C-l") 'lisp-load-file)
  (define-key clojure-mode-map (kbd "C-c C-n") 'lisp-eval-form-and-next)
  (define-key clojure-mode-map (kbd "C-c C-p") 'lisp-eval-paragraph)
  (define-key clojure-mode-map (kbd "C-c C-r") 'lisp-eval-region)
  (define-key clojure-mode-map (kbd "C-c C-v") 'lisp-show-variable-documentation)
  (define-key clojure-mode-map (kbd "C-c C-z") 'run-clojure)
  (define-key clojure-mode-map (kbd "C-M-x") 'lisp-eval-defun)
  (define-key clojure-mode-map (kbd "C-M-q") 'indent-sexp))
(add-hook 'clojure-mode-hook 'add-clojure-eval)



