;;; init.el -*- lexical-binding: t; -*-

;; Functions
(defun python-doc ()
  (interactive)
  (setq-local devdocs-current-docs '("python-3.11")))

(add-hook 'python-mode-hook #'eldoc-mode)

(when (fboundp #'devdocs-lookup)
  (add-hook 'python-mode-hook #'python-doc))

(leaf anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook #'anaconda-mode)

  ;;  Move anaconda python installation directory to user var/
  (customize-set-variable
   'anaconda-mode-installation-directory
   (expand-file-name "anaconda-mode" lt/config-var-dir))
  )

(leaf blacken
  :ensure t
  :config
  (add-hook 'python-mode-hook #'blacken-mode)
  )
(leaf pyvenv
  :ensure t
  :config
  (add-hook 'python-mode-hook #'pyenv-mode)
  (add-hook 'python-mode-hook #'pyenv-tracking-mode)

  ;;  restart python when the virtual environment changes
  (add-hook 'pyenv-post-activate-hooks #'pyenv-restart-python)

  ;; default to the commonly used "venv" folder for the virtual
  ;; environment
  (customize-set-variable 'pyvenv-default-virtual-env-name "venv")
)

;;; Python mode
(customize-set-variable 'python-indent-guess-indent-offset-verbose nil)

;;; numpydoc
(leaf numpydoc
  :ensure t
  :config
  (customize-set-variable 'numpydoc-insert-examples-block nil)
  (customize-set-variable 'numpydoc-template-long nil)
)

(provide 'lt-python)
