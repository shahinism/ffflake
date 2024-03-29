;;; sh-completion.el --- Personal Completion Configuration -*- lexical-binding: t; -*-
(provide 'sh-development)

;; TODO add autosave mode
;; TODO configure temp file directory
;; Exec Path From Shell
; Do it on window systems and Unix based environment
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; direnv
(use-package direnv
  :init
  (direnv-mode))

;; Show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

(use-package undo-fu-session
  :hook (after-init . global-undo-fu-session-mode)
  :custom
  (undo-fu-session-directory (sh-cache-dir "undo-fu-session"))
  (undo-fu-session-linear nil)
  (undo-fu-session-file-limit nil)
  (undo-fu-session-incompatible-files '("\\.gpg$"
                                        "/COMMIT_EDITMSG\\'"
                                        "/git-rebase-todo\\'"))
  :config
  (when (executable-find "zstd")
    (setq undo-fu-session-compression 'zstd))
  )

(use-package vundo
  :commands (vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t)
  :bind
  (("C-c u" . vundo)
   :map vundo-mode-map
        ("l" . vundo-forward)
        ("h" . vundo-backward)
        ("n" . vundo-next)
        ("p" . vundo-previous)
        ("q" . vundo-quit))
  :config
  )

;; -> Linum
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; -> Devdocs
(use-package devdocs
  ;; TODO extract a function instead of this lambda
  :hook ((python-mode . (lambda () (setq-local devdocs-current-docs '("python-3.11"))))
         (emacs-lisp-mode . (lambda () (setq-local devdocs-current-docs '("elisp"))))
         (sh-mode . (lambda () (setq-local devdocs-current-docs '("bash"))))
         (nix-mode . (lambda () (setq-local devdocs-current-docs '("nix"))))
         (rust-mode . (lambda () (setq-local devdocs-current-docs '("rust"))))
         (go-mode . (lambda () (setq-local devdocs-current-docs '("go")))))
  :bind
  ("C-c d" . devdocs-lookup))

(use-package eglot
  :commands eglot eglot-ensure
  :hook
  (python-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  (typescript-mode . eglot-ensure)
  (js-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-stay-out-of 'company)
  ;; Shutdown server when last managed buffer is killed
  (setq eglot-sync-connect 1
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
        ;;      its popup rule causes eglot to steal focus too often.
        eglot-auto-display-help-buffer nil)
  )

(use-package projectile
  :blackout
  :init
  (projectile-mode +1))

(use-package tree-sitter
  :init
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package flycheck
  :init
  (global-flycheck-mode)
  :bind
  (("M-n" . flycheck-next-error)
   ("M-p" . flycheck-previous-error))
  :custom
  (flycheck-display-errors-delay 0))

(use-package magit)
(use-package markdown-mode)
(use-package markdown-toc)
(use-package grip-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :config
  (require 'auth-source)
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential))))

;; better export suggested by grip-mode
(use-package ox-gfm
  :after org
  :config
  (require 'ox-gfm nil t))

;; font-lock annotations like TODO in the source code
(use-package hl-todo
  :init
  (global-hl-todo-mode 1))

(use-package dumb-jump) ;; TODO config dumb-jump binding
(use-package terraform-mode)
(use-package nix-mode)
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker)

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Fix trailing spaces but only in modified lines
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package yaml-mode)

(use-package indent-guide
  :hook (prog-mode . indent-guide-mode))

;; Code folding
(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))

;;
;; -> Common Lisp
;;
(use-package sly
  :custom
  (inferior-lisp-program "sbcl"))
;;
;; -> Exercism
;;
(use-package exercism)

;;
;; -> SQL
;;
(use-package sql
  :mode ("\\.sql\\'" "\\.ksql\\'"))

;; TODO add https://github.com/purcell/sqlformat

;; -> TypeScript
(use-package typescript-mode
  :after tree-sitter
  :mode "\\.ts\\'"
  :hook
  (typescript-mode . tree-sitter-hl-mode)
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  (setq typescript-indent-level 2)
  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; -> Go
(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (go-mode . tree-sitter-hl-mode)
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

;; -> Nu
(use-package nushell-mode
  :mode "\\.nu\\'"
  :hook
  (nu-mode . tree-sitter-hl-mode))

;; -> PlantUML
(use-package plantuml-mode
  :mode "\\.puml\\'")

;; -> Shell
(use-package eat
  :straight '(eat
              :type git
              :host codeberg
              :repo "akib/emacs-eat"
              :files ("*.el" ("term" "term/*.el") "*.texi"
                      "*.ti" ("terminfo/e" "terminfo/e/*")
                      ("terminfo/65" "terminfo/65/*")
                      ("integration" "integration/*")
                      (:exclude ".dir-locals.el" "*-tests.el"))))

;; -> Rust
;; NOTE requires rustfmt
(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t)
  :hook
  (rust-mode . eglot-ensure)
  (rust-mode . tree-sitter-hl-mode)
  (rust-mode . prettify-symbols-mode)
  )

;; TODO setup key bindings
(use-package cargo-mode
  :hook
  (rust-mode . cargo-minor-mode))

;; -> Python
(use-package anaconda-mode
  :hook
  (python-mode . anaconda-mode)
  :config

  ;;  Move anaconda python installation directory to user var/
  (customize-set-variable
   'anaconda-mode-installation-directory
   (expand-file-name "anaconda-mode" lt/config-var-dir))
  )

(use-package blacken
  :hook
  (python-mode . blacken-mode))

(use-package numpydoc
  :custom
  (numpydoc-insert-examples-block nil)
  (numpydoc-template-long nil))

(use-package pyimport
  :bind
  (:map python-mode-map
        ("C-c C-i" . pyimport-insert-missing)))

;; Requires `importmagic' and `epc' packages to be available in the
;; `PATH'.
(use-package importmagic
  :disabled  ; drastically slows down the instance!
  :bind
  (:map importmagic-mode-map
        ("C-c C-f" . importmagic-fix-symbol-at-point))
  :hook
  (python-mode-hook 'importmagic-mode)
  )

(use-package pyimpsort
  :bind
  (:map python-mode-map
        ("C-c C-u" . pyimpsort-buffer)))

(use-package jq-mode
  :mode "\\.jq\\'")
