;;; sh-code.el --- Emacs Code to Infinity -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(provide 'sh-code)

;; Show the name of the current function definition in the modeline.
(require 'which-func)
(which-function-mode 1)

;; Line numbers (relative of course).
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Highlight the current line.
(global-hl-line-mode 1)

(defun sh/set-local-devdocs (doc)
  "Set the local DEVDOCS to DOC."
  (interactive "sLocal DEVDOCS: ")
  (setq-local devdocs-current-docs doc))

(leaf devdocs
  :doc "Search the local devdocs."
  :url "https://github.com/astoff/devdocs.el"
  :ensure t
  :hook
  (python-mode-hook . (lambda () (sh/set-local-devdocs "python-3.12")))
  (emacs-lisp-mode-hook . (lambda () (sh/set-local-devdocs "elisp")))
  (sh-mode-hook . (lambda () (sh/set-local-devdocs "bash")))
  (nix-mode-hook . (lambda () (sh/set-local-devdocs "nix")))
  (rust-mode-hook . (lambda () (sh/set-local-devdocs "rust")))
  (go-mode-hook . (lambda () (sh/set-local-devdocs "go")))
  :bind
  ("C-c d" . devdocs-lookup))

(leaf eglot
  :doc "Client for Language Server Protocol servers"
  :url "https://github.com/joaotavora/eglot/"
  :req "emacs-29"
  :tag "builtin"
  :commands eglot eglot-ensure
  :hook
  (python-mode-hook . eglot-ensure)
  (go-mode-hook . eglot-ensure)
  (rust-mode-hook . eglot-ensure)
  (js-mode . eglot-ensure)
  :custom
  (eglot-sync-connect . 1)
  (eglot-connect-timeout . 10)
  (eglot-autoshutdown . t)
  (eglot-send-changes-idle-time . 0.5)
  ;; NOTE disable eglot-auto-display-help-buffer because :select t in
  ;;      its popup rule causes eglot to steal focus too often.
  (eglot-auto-display-help-buffer . nil))

(leaf major-mode-hydra
  :doc "Spacemacs-inspired major mode leader key powered by Hydra"
  :url "https://github.com/jerrypnz/major-mode-hydra.el"
  :ensure t)

(leaf project
  :doc "Manage and navigate projects in Emacs easily"
  :tag "builtin"
  :pretty-hydra
  ((:color teal :quit-key "q")
   ("Project"
    (("f" project-find-file "find file")
     ("d" project-dired "dired")
     ("b" consult-project-buffer "switch to buffer")
     ("p" project-switch-project "switch project")
     ("k" project-kill-buffers "kill buffers"))
    "Find"
    (("g" project-find-regexp "find regexp")
     ("r" project-query-replace-regexp "query replace regexp"))
    "Action"
    (("s" project-shell "shell")
     ("e" project-eshell "eshell")
     ("c" project-compile "compile")
     ("x" project-execute-extended-command "execute extended command")
     ("a" consult-ag "ag")
     ("v" project-vc-dir "vc dir"))
    ))
  )

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :url "https://github.com/magit/magit"
  :ensure t
  :commands magit-status)

(leaf git-gutter
  :doc "Port of Sublime Text plugin GitGutter"
  :url "https://github.com/emacsorphanage/git-gutter"
  :ensure t
  :hook
  (prog-mode-hook . git-gutter-mode)
  :bind
  ("C-x g" . git-gutter/body)
  :pretty-hydra
  ((:color red :hint nil :quit-key "q")
   ("Jump"
   (("n" git-gutter:next-hunk "next hunk")
    ("p" git-gutter:previous-hunk "previous hunk"))
   "Action"
   (("s" git-gutter:stage-hunk "stage hunk" :color blue)
    ("r" git-gutter:revert-hunk "revert hunk" :color blue)
    ("d" git-gutter:popup-hunk "popup hunk" :color blue)
    ("R" git-gutter:set-start-revision "set start revision" :color blue))
   ))
  )

;; TODO git-gutter

(leaf markdown-mode
  :doc "Major mode for Markdown-formatted text"
  :url "http://jblevins.org/projects/markdown-mode/"
  :ensure t)

(leaf markdown-toc
  :doc "Generate a table of contents for markdown files"
  :url "https://github.com/ardumont/markdown-toc"
  :ensure t)

(leaf grip-mode
  :doc "Instant Github-flavored Markdown/Org preview using grip"
  :url "https://github.com/seagle0128/grip-mode"
  :ensure t
  :bind (:markdown-mode-command-map
         ("p" . grip-mode))
  :config
  (require 'auth-source)
    (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential)))
  )

(leaf ox-gfm
  :doc "Github Flavored Markdown Back-End for Org Export Engine. Suggested by ~grip-mode~."
  :url "https://github.com/larstvei/ox-gfm"
  :after org
  :ensure t
  :config
  (require 'ox-gfm nil t))

(leaf hl-todo
  :doc "Highlight TODO keywords"
  :url "https://github.com/tarsius/hl-todo"
  :ensure t
  :init
  (global-hl-todo-mode))

;; TODO dumb-jump
(leaf terraform-mode
  :doc "Major mode for terraform configuration files"
  :url "https://github.com/hcl-emacs/terraform-mode"
  :ensure t)

(leaf nix-mode
  :doc "Major mode for editing Nix expressions"
  :url "https://github.com/NixOS/nix-mode"
  :ensure t)

(leaf dockerfile-mode
  :doc "Major mode for editing Docker's Dockerfiles"
  :url "https://github.com/spotify/dockerfile-mode"
  :ensure t)

(leaf docker-compose-mode
  :doc "Major mode for editing Docker Compose files"
  :url "https://github.com/meqif/docker-compose-mode"
  :ensure t)

;; TODO check me out
(leaf docker
  :doc "Emacs Docker client"
  :url "https://github.com/Silex/docker.el"
  :ensure t)

;; TODO check TRAMP

(leaf rainbow-mode
  :doc "Colorize color names in buffers"
  :url "https://elpa.gnu.org/packages/rainbow-mode.html"
  :ensure t
  :hook
  (prog-mode-hook . rainbow-mode))

(leaf rainbow-delimiters
  :doc "Highlight delimiters such as parentheses, brackets or braces according to their depth."
  :url "https://github.com/Fanael/rainbow-delimiters"
  :ensure t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(leaf ws-butler
  :doc "Unobtrusively trim extraneous white-space *ONLY* in lines edited"
  :url "https://github.com/lewang/ws-butler"
  :ensure t
  :hook
  (prog-mode-hook . ws-butler-mode))

(leaf yaml-mode
  :doc "Major mode for editing YAML files"
  :url "https://github.com/yoshiki/yaml-mode"
  :ensure t)

(leaf indent-guide
  :doc "Show vertical lines to guide indentation"
  :url "https://github.com/zk-phi/indent-guide"
  :ensure t
  :hook
  (prog-mode-hook . indent-guide-mode))

;; TODO enable code folding
(leaf sql
  :doc "SQL mode"
  :tag "builtin"
  :mode ("\\.sql\\'" "\\.ksql\\'"))

(leaf typescript-mode
  :doc "Major mode for editing typescript"
  :url "https://github.com/emacs-typescript/typescript.el"
  :ensure t
  :mode "\\.ts\\'"
  :custom
  (typescript-indent-level . 2)
  :config
  ;; we choose this instead of tsx-mode so that eglot can
  ;; automatically figure out language for server see
  ;; https://github.com/joaotavora/eglot/issues/624 and
  ;; https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  )

(leaf go-mode
  :doc "Major mode for the Go programming language"
  :url "https://github.com/dominikh/go-mode.el"
  :ensure t
  :mode "\\.go\\'"
  :custom
  (gofmt-command . "goimports")
  :hook
  (before-save-hook . gofmt-before-save))

;; TODO check tree-sitter

(leaf nushell-mode
  :doc "Major mode for editing Nushell scripts"
  :url "https://github.com/mrkkrp/nushell-mode"
  :ensure t
  )

(leaf plantuml-mode
  :doc "Major mode for PlantUML"
  :url "https://github.com/skuro/plantuml-mode"
  :ensure t
  )

;; TODO checkout eat shell
(leaf rust-mode                ;; NOTE this requires rustfmt to be installed.
  :doc "A major emacs mode for editing Rust source code"
  :url "https://github.com/rust-lang/rust-mode"
  :ensure t
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save . t)
  :hook
  (rust-mode-hook . eglot-ensure)
  (rust-mode-hook . prettify-symbols-mode))

;; TODO setup keybindings
(leaf cargo-mode
  :doc "Minor mode for Cargo, Rust's package manager."
  :url "https://github.com/ayrat555/cargo-mode"
  :ensure t
  :hook
  (rust-mode-hook . cargo-minor-mode))

;; TODO setup keybindings
(leaf anaconda-mode
  :doc "Code navigation, documentation lookup and completion for Python"
  :url "https://github.com/pythonic-emacs/anaconda-mode"
  :ensure t
  :hook
  (python-mode-hook . anaconda-mode)
  :config
    ;;  Move anaconda python installation directory to user var/
  (customize-set-variable
   'anaconda-mode-installation-directory
   (expand-file-name "anaconda-mode" sh/var-dir))
  )

(leaf blacken
  :doc "Blacken Python source code"
  :url "https://github.com/pythonic-emacs/blacken"
  :ensure t
  :hook
  (python-mode-hook . blacken-mode))

(leaf pyimport
  :doc "Import completion for Python"
  :url "https://github.com/Wilfred/pyimport"
  :ensure t
  :pretty-hydra
  ;; TODO bind this hydra
  ((:color teal :quit-key "q")
   ("Imports"
    (("r" pyimport-remove-unused "remove unused imports")
     ("i" pyimport-insert-missing "insert missing dependencies"))
    ))
  )

(leaf hs-minor-mode
  :doc "Hide and show blocks of text"
  :tag "builtin"
  :hook
  (prog-mode-hook . hs-minor-mode)
  :bind
  ("C-c h" . hs-minor-mode/body)
  :pretty-hydra
  ((:color teal :quit-key "q")
   ("Block"
    (("h" hs-hide-block "hide block")
     ("s" hs-show-block "show block")
     ("t" hs-toggle-hiding "toggle hiding"))
    "All"
    (("H" hs-hide-all "hide all")
     ("S" hs-show-all "show all"))
    "Level"
    (("l" hs-hide-level "hide level")
    ))
   )
  )

(leaf feature-mode
  :doc "Major mode for editing Gherkin files"
  :url "https://github.com/emacsmirror/feature-mode"
  :ensure t
  :config
  (defun turn-on-orgtbl ()
    "Turn on orgtbl-mode."
    ;; NOTE this is a workaround for a bug in feature-mode
    ))

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :url "https://www.flycheck.org/en/latest/"
  :ensure t
  :hook
  (prog-mode-hook . flycheck-mode)
  :bind
  ("C-c e" . flycheck/body)
  :custom
  (flycheck-check-syntax-automatically . '(save mode-enabled))
  (flycheck-idle-change-delay . 0.5)
  (flycheck-display-errors-delay . 0.5)
  :pretty-hydra
  ((:color teal :quit-key "q")
   ("Errors"
    (("n" flycheck-next-error "next error")
     ("p" flycheck-previous-error "previous error")
     ("l" flycheck-list-errors "list errors"))
    "Checker"
     (("d" flycheck-describe-checker "describe checker")
     ("m" flycheck-mode "toggle mode")
     ("s" flycheck-select-checker "select checker")
     ("c" flycheck-clear "clear")
     ("C" flycheck-buffer "check buffer")
     ("v" flycheck-verify-setup "verify setup"))
    ))
  )

(leaf flycheck-posframe
  :doc "Flycheck errors display in posframe"
  :url "https://github.com/alexmurray/flycheck-posframe"
  :ensure t
  :hook
  (flycheck-mode-hook . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults))

(leaf flycheck-rust
  :doc "Flycheck: Rust additions and Cargo support"
  :url "https://github.com/flycheck/flycheck-rust"
  :ensure t
  :hook
  (rust-mode-hook . flycheck-rust-setup))
;;; sh-code.el ends here
