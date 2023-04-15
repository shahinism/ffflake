;;; init.el -*- lexical-binding: t; -*-

(leaf vertico
  :doc "Completion interface"
  :url "https://github.com/minad/vertico/"
  :global-minor-mode vertico-mode
  :ensure t
  :custom
  (vertico-cycle . t)
  (vertico-count . 18)
  :config
  (require 'vertico-directory))

(leaf consult
  :doc "Generate completion candidates and provide commands for completion"
  :url "https://github.com/minad/consult"
  :ensure t
  :bind
  ("M-y" . consult-yank-pop)
  ("C-s" . consult-line)
  :custom (consult-async-min-input . 1)
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history)
  (setq completion-in-region-function #'consult-completion-in-region))

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
  :custom
  (marginalia-annotators . '(marginalia-annotators-heavy marginalia-annotators-light nil))
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
  :ensure t
  :config
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(leaf corfu
  :ensure t
  :custom
  (corfu-cycle . t)
  (corfu-auto . t)
  (corfu-auto-prefix . 2)
  (corfu-auto-delay . 0.0)
  (corfu-echo-documentation . 0.1)
  :config
  (require 'corfu-popupinfo)
  (require 'corfu)

  (unless (display-graphic-p)
    (require 'corfu-terminal)
    (corfu-terminal-mode +1))

  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (eldoc-add-command #'corfu-insert))

(leaf cape
  :ensure t
  :init
  ;; Add useful defaults completion sources from cape
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :config
  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode)))
  )

(leaf kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face . 'corfu-default) ; to comput blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf tabnine-capf
  :after cape
  :commands (tabnine-capf-install-binary
             tabnine-capf-restart-server)
  :straight (tabnine-capf :type git :host github :repo "50ways2sayhard/tabnine-capf" :files ("*.el" "*.sh"))
  :hook (kill-emacs . tabnine-capf-kill-process)
  :config
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point))

(setq-default abbrev-mode 1)

(provide 'lt-completion)
