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

;;; sh-code.el ends here
