;;; init.el -*- lexical-binding: t; -*-

(server-start)

;; Profile Emacs startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))
(defvar native-comp-deferred-compilation-deny-list nil)
;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
             :custom (straight-use-package-by-default t))
;; leaf
(eval-and-compile
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; Add modules folder to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;; Set default coding system
(set-default-coding-systems 'utf-8)
(customize-set-variable 'visible-bell 1) ; Visually flash instead of beep!
(customize-set-variable 'large-file-warning-threshold (* 100 1000 1000)) ; Change to around ~100 MB

;; Define the user configuration var and etc folders and ensure they
;; exist:
(defvar lt/config-etc-dir (expand-file-name "etc/" user-emacs-directory)
  "The user's configuration etc/ folder")
(defvar lt/config-var-dir (expand-file-name "var/" user-emacs-directory)
  "The user's configuration var/ folder")

(unless (file-exists-p lt/config-etc-dir)
  (mkdir lt/config-etc-dir))

(unless (file-exists-p lt/config-var-dir)
  (mkdir lt/config-var-dir))

;; Require modules in use
(when (eq system-type 'gnu/linux)
  (require 'lt-linux))

(require 'lt-default)
;; (require 'lt-evil)
(require 'lt-meow)
(require 'lt-completion)
(require 'lt-editing)
(require 'lt-programming)
(require 'lt-python)
(require 'lt-ui)
(require 'lt-env)
(require 'lt-typescript)
(require 'lt-kotlin)
(require 'lt-shell)
(require 'lt-web)
(require 'lt-go)
(require 'lt-note)
(require 'lt-lsp)
(require 'lt-hydra)

