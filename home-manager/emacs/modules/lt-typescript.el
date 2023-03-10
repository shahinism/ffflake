;;; init.el -*- lexical-binding: t; -*-

(leaf js2-refactor :ensure t)

;; Functions
(defun typescript-doc ()
  (interactive)
  (setq-local devdocs-current-docs '("typescript")))

(leaf typescript-mode
  :ensure t
  :config

  ;; Hooks
  (when (fboundp #'devdocs-lookup)
    (add-hook 'typescript-mode-hook #'typescript-doc))
  )

(provide 'lt-typescript)
