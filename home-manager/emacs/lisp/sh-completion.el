;;; sh-completion.el --- Personal Completion Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal completion configuration; Anything related to completion
;; narrowing, and finding based on user inputs, are placed in this
;; module.


;;; Code:
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              ("C-d" . vertico-directory-enter))

  :custom
  (vertico-cycle t)
  :init
  (vertico-mode 1)
  :config
  (require 'vertico-directory))

(use-package marginalia
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode 1))

(use-package embark-consult)

(use-package consult
  :bind
  (("C-s" . 'consult-line)
   ("M-y" . 'consult-yank-pop)
   :map minibuffer-local-map
   ("C-r" . 'consult-history)))

(use-package consult-ag
  :after consult)

(use-package embark
  :bind
  (("C-." . 'embark-act)
   ([remap describe-bindings] . 'embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :after vertico
  :straight (:files (:defaults "extensions/*"))
  :custom
  (corfu-cycle t)                 ; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)           ; Complete when you input 2 chars
  (corfu-auto-delay 0.0)          ; Wait 0.2s before showing completion
  (corfu-echo-documentation 0.25) ; Show documentation after 0.25s
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (global-corfu-mode)
  (eldoc-add-command #'corfu-insert))

(use-package corfu-terminal
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package cape
  :hook
  (eshell-mode-hook . (lambda ()
                        (setq-local corfu-quit-at-boundary t
                                    corfu-quit-no-match t
                                    corfu-auto :if-nil)
                        (corfu-mode 1)))
  :custom
  (completion-at-point-functions #'cape-file)
  (completion-at-point-functions #'cape-dabbrev)
  :config
  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete doesn't write into the buffer
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package copilot
  :hook (prog-mode . copilot-mode)
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-;") 'copilot-accept-completion-by-word))

(use-package emmet-mode
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode)
         (web-mode . emmet-mode)
         (html-mode . emmet-mode)))

(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.php\\'"
         "\\.xml\\'")
  :config
  (progn
    (setq web-mode-engines-alist '(("hugo" . ".*hugo.*\\(html\\|xml\\)\\'")))))

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el"))
  :config
  (setq chatgpt-shell-openai-key
      (lambda ()
        (auth-source-pick-first-password :host "api.openai.com"))
      chatgpt-shell-streaming t
      chatgpt-shell-model-version "gpt-3.5-turbo"
      chatgpt-shell-request-timeout 300000))

(provide 'sh-completion)
