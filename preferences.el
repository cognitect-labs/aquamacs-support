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

(use-package exec-path-from-shell
             :ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package paredit
             :ensure t
             :hook ((clojure-mode emacs-lisp lisp-mode lisp-interaction-mode) . paredit-mode)
             :bind (:map paredit-mode-map
                         ("M-[" . paredit-wrap-square)
                         ("M-{" . paredit-wrap-curly)
                         ("M-<right>" . forward-sexp)
                         ("M-<left>" . backward-sexp)
                         ("M-<up>" . backward-up-list)
                         ("M-<down>" . down-list)
                         ("<A-return>" . paredit-newline)))

(use-package clojure-mode
             :ensure t
             :bind (:map clojure-mode-map
                         ("C-M-x" . lisp-eval-defun)
                         ("C-x C-e" . lisp-eval-last-sexp)
                         ("C-c C-e" . lisp-eval-last-sexp)
                         ("C-c C-z" . run-clojure)
                         ("C-c C-r" . lisp-eval-region)
                         ("C-x C-e" . lisp-eval-last-sexp)
                         ("C-c C-a" . lisp-show-arglist)
                         ("C-c C-c" . lisp-compile-defun)
                         ("C-c C-d" . lisp-describe-sym)
                         ("C-c C-e" . lisp-eval-defun)
                         ("C-c C-f" . lisp-show-function-documentation)
                         ("C-c C-k" . lisp-compile-file)
                         ("C-c C-l" . lisp-load-file)
                         ("C-c C-n" . lisp-eval-form-and-next)
                         ("C-c C-p" . lisp-eval-paragraph)
                         ("C-c C-r" . lisp-eval-region)
                         ("C-c C-v" . lisp-show-variable-documentation)
                         ("C-c C-z" . run-clojure)
                         ("C-M-x" . lisp-eval-defun)
                         ("C-M-q" . indent-sexp)))


(setq inferior-lisp-program "clojure")

(defun run-clojure (cmd)
  (interactive (list (read-from-minibuffer "Command:" "clojure")))
  (let ((default-directory (if (fboundp 'clojure-project-root-path)
			                   (clojure-project-root-path)
			                 default-directory))
	    cb (curent-buffer))
    (run-lisp cmd)
    (switch-to-buffer cb)
    (switch-to-buffer-other-window "*inferior-lisp*")))
