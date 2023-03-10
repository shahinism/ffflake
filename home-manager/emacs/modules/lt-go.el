;;; init.el -*- lexical-binding: t; -*-

(leaf go-mode
  :doc "Go development environment"
  :url "https://github.com/dominikh/go-mode.el"
  :ensure t
  :mode "\\.go\\'"
  :hook
  (go-mode-hook
   . (lambda ()
       (setq-local devdocs-current-docs '("go"))))
  :bind
  (:go-mode-map
   ("C-c C-n" . go-run)
   ("C-c v"   . go-mod-vendor))
  :preface
  (defun go-mod-vendor ()
    "Run 'go mod vendor' at repository root."
    (interactive)
    (progn
      (call-process-shell-command (concat "cd " (vc-root-dir) "; go mod vendor") nil 0)
      (message "Run 'go mod vendor'!"))))

(leaf gotest
  :doc "Run Go unit-tests"
  :url "https://github.com/nlamirault/gotest.el"
  :ensure t
  :require t
  :bind
  (:go-mode-map
   ("C-c t" . go-test-clean-and-current-test)
	 ("C-c f" . go-test-clean-and-current-file)
   ("C-c T" . go-test-current-test)
	 ("C-c F" . go-test-current-file)
	 ("C-c a" . go-test-current-project))
  :preface
  (defun go-test-clean-and-current-test()
    (interactive)
    (progn
      (call-process-shell-command "go clean -testcache" nil 0)
      (go-test-current-test)))
  (defun go-test-clean-and-current-file()
    (interactive)
    (progn
      (call-process-shell-command "go clean -testcache" nil 0)
      (go-test-current-file))))

(leaf go-gen-test
  :doc "Generate tests for go code"
  :url "https://github.com/s-kostyaev/go-gen-test"
  :ensure t)

(leaf go-eldoc
  :doc "Show eldoc for Go functions"
  :url "https://github.com/emacsorphanage/go-eldoc"
  :ensure t
  :hook (go-mode-hook . go-eldoc-setup))

(leaf go-tag
  :doc "Generate & Edit field tags for golang struct fields"
  :url "https://github.com/brantou/emacs-go-tag"
  :ensure t)

(provide 'lt-go)
