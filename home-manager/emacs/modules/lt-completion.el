;;; init.el -*- lexical-binding: t; -*-

(leaf vertico
  :doc "Completion interface"
  :url "https://github.com/minad/vertico/"
  :global-minor-mode vertico-mode
  :ensure t
  :custom
  (vertico-cycle . t)
  (vertico-count . 18))

(leaf consult
  :doc "Generate completion candidates and provide commands for completion"
  :url "https://github.com/minad/consult"
  :ensure t
  :bind
  ("M-y"   . consult-yank-pop)
  ("C-M-s" . consult-line)
  :custom (consult-async-min-input . 1))

(leaf consult-flycheck
  :doc "Consult integration for Flycheck"
  :url "https://github.com/minad/consult-flycheck"
  :ensure t)

(leaf affe
  :doc "Asynchronous Fuzzy Finder"
  :url "https://github.com/minad/affe"
  :ensure t)

(leaf consult-ghq
  :doc "Consult integration for ghq (with affe)"
  :url "https://github.com/tomoya/consult-ghq"
  :ensure t)

(leaf consult-custom
  :doc "Custom functions to search org documents"
  :after affe
  :require affe
  :preface
  (defun consult-find-doc ()
    "Search org files in the private document directory."
    (interactive)
    (let ((affe-find-command "fdfind --ignore-case --extension org --no-ignore ."))
      (funcall #'affe-find org-directory)))
  (defun consult-grep-doc ()
    "Search text in the private document directory"
    (interactive)
    (let ((affe-grep-command "rg --null --color=never --max-columns=1000 --ignore-case --no-ignore --no-heading --line-number -v ^$ ."))
      (funcall #'affe-grep org-directory))))

(leaf marginalia
  :doc "Explain details of the consult candidates"
  :url "https://github.com/minad/marginalia"
  :global-minor-mode marginalia-mode
  :ensure t
  :custom-face
  (marginalia-documentation . '((t (:foreground "#6272a4")))))

(leaf orderless
  :doc "Completion style that matches multiple regexps"
  :url "https://github.com/oantolin/orderless"
  :ensure t
  :preface
  (defun flex-if-apostrophe (pattern _index _total)
    (when (string-suffix-p "'" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))
  :custom
  (completion-styles           . '(orderless partial-completion))
  (orderless-style-dispatchers . '(flex-if-apostrophe
                                   without-if-bang))
  (completion-category-overrides '((eglot (styles . (orderless flex))))))

(leaf embark
  :doc "Mini-Buffer Actions Rooted in Keymaps Resources"
  :url "https://github.com/oantolin/embark"
  :ensure t
  :bind*
  ("M-a" . embark-act)
  :custom
  (prefix-help-command . #'embark-prefix-help-command)
  :config
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(leaf embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(leaf orderless
  ;; :demand t
  :config
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(leaf yasnippet
  ;; :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(leaf yasnippet-snippets
  ;; :defer t
  :after yasnippet)

(leaf company
  :ensure t
  :custom
  (company-idle-delay . 0)
  (company-minimum-prefix-length . 1)
  (company-show-numbers . t)
  :hook (prog-mode-hook . company-mode))

(leaf company-tabnine
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends #'company-tabnine))

(leaf company-box
  :ensure t
  :if (display-graphic-p)
  :after company
  :hook (company-mode . company-box-mode))

(setq-default abbrev-mode 1)

(provide 'lt-completion)
