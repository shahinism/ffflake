;;; sh-completion.el --- Personal Completion Configuration -*- lexical-binding: t; -*-
(provide 'sh-development)


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
