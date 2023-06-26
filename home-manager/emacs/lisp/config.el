;;; init.el -*- lexical-binding: t; -*-

(provide 'config)

;;;;;;;;;;;;;;;;;;;;;;;;; Defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revert Dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Shorten the confirmation answers. Prefer setting the config
;; variable available on Emacs 28.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; Turn on recentf mode
(add-hook 'after-init-hook #'recentf-mode)
(customize-set-variable 'recentf-save-file
                        (expand-file-name "recentf" lt/config-var-dir))

;; Do not save duplicates in the kill ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Enable savehist-mode for the command history
(savehist-mode 1)
(customize-set-variable 'savehist-file
                        (expand-file-name "history" lt/config-var-dir))

;; backups
(setq make-backup-files nil
      auto-save-default nil)

;; Remember the position in the buffer
(save-place-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tools ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package blackout)

(use-package move-border
  :straight (:host github :repo "ramnes/move-border" :branch "master"))

(use-package zoom-frm
  :straight (:host github :repo "emacsmirror/zoom-frm" :branch "master"))

;; Helper commands
(use-package crux)

;; GPG configuration
(use-package pinentry
  :config
  (pinentry-start))

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parentheses
(electric-pair-mode 1)

(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; Use settings from .editorconfig file when present
(use-package editorconfig
  :blackout " "
  :config
  (editorconfig-mode 1))

;; Flyspell + UI
(use-package flyspell
  :hook
  (prog-mode-hook . flyspell-prog-mode)
  ((markdown-mode-hook git-commit-mode-hook) . flyspell-mode)
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

(use-package flyspell-correct
  :bind
  ("C-M-i" . flyspell-correct-at-point)
  :custom
  (flyspell-correct-interface . #'flyspell-correct-completing-read))

;; Anzu
; Displays current match and total matches information
(use-package anzu
  :bind ("M-r" . anzu-query-replace-regexp))

(use-package ace-window)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; User Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package all-the-icons)

;;;; Font
(defun shahin/ui--set-default-font (spec)
  "Set the default font based on SPEC
SPEC is expected to be a plist with the same key names as
accepted by `set-default-attribute'."
  (when spec
    (apply 'set-face-attribute 'default nil spec)))

;; TODO set font only when availabe
(shahin/ui--set-default-font '(:font "FiraCode Nerd Font" :weight regular :height 120))

;;;; doom-modeline
(use-package doom-modeline
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar 6)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  :init
  (add-hook 'after-init-hook #'doom-modeline-mode))

;;;; helpful
;; Make `describe-*` screens more helpful!
(use-package helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ("C-h f" . helpful-function)
  ("C-h k" . describe-keymap))

;;;; which-key
(use-package which-key
  :blackout
  :init
  (which-key-mode))

;;;; Theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t))

(use-package zenburn-theme
  :disabled ;; With Embark description doesn't play well. The same
            ;; goes with `doom-zenburn'.
  :config
  (setq
   ;; use variable-pitch fonts for some headings and titles
   zenburn-use-variable-pitch t
   ;; scale headings in org-mode
   zenburn-scale-headlines t
   ;; scale headings in outline-mode
   zenburn-scale-outline-headlines t)
  (load-theme 'zenburn t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package hydra)
(use-package major-mode-hydra
  :after hydra
  :functions pretty-hydra)

;; TODO check me
(pretty-hydra-define hydra-search
  (:title " Search" :color blue :quit-key "q" :foreign-keys warn)
  ("Buffer"
   (("l" consult-line "line")
    ("o" consult-outline "outline")
    ("m" consult-imenu "imenu"))
   "Project"
   (("f" affe-find "find")
    ("r" affe-grep "grep"))
   "Document"
   (("df" consult-find-doc "find")
    ("dd" consult-grep-doc "grep"))))

(pretty-hydra-define hydra-file
  (:title " File" :color blue :quit-key "q")
  ("Open"
   (("f" find-file "Open File")
    ("r" recentf "Find recent file"))
   "Operation"
   (("n" crux-rename-file-and-buffer "Rename file and buffer"))))

(pretty-hydra-define hydra-window
  (:title "  Windows" :color blue :quit-key "q")
  ("Actions"
   (("TAB" other-window "switch")
    ("d" ace-delete-window "delete" :color amaranth)
    ("m" ace-delete-other-windows "maximize")
    ("s" ace-swap-window "swap")
    ("a" ace-select-window "select"))
   "Move"
   (("h" windmove-left "←")
    ("j" windmove-down "↓")
    ("k" windmove-up "↑")
    ("l" windmove-right "→"))
   
   "Resize"
   (("H" move-border-left "←" :color amaranth)
    ("J" move-border-down "↓" :color amaranth)
    ("K" move-border-up "↑" :color amaranth)
    ("L" move-border-right "→" :color amaranth)
    ("n" balance-windows "balance")
    ("f" toggle-frame-fullscreen "toggle fullscreen"))

   "Split"
   (("/" split-window-right "horizontally")
    ("?" split-window-horizontally-instead "horizontally instead")
    ("-" split-window-below "vertically")
    ("_" split-window-vertically-instead "vertically instead"))

   "Zoom"
   (("i" zoom-in "in" :color amaranth)
    ("o" zoom-out "out" :color amaranth)
    ("0" (lambda ()
           (interactive)
           (zoom-in/out 0)) "reset"))))

(pretty-hydra-define hydra-toggles
  (:title "  Toggle" :color amaranth :quit-key "q")
  ("Basic"
   (("n" linum-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("W" whitespace-cleanup-mode "whitespace cleanup" :toggle t)
    ("r" rainbow-mode "rainbow" :toggle t)
    ("L" page-break-lines-mode "page break lines" :toggle t))
   "Highlight"
   (("s" symbol-overlay-mode "symbol" :toggle t)
    ("l" hl-line-mode "line" :toggle t)
    ("x" highlight-sexp-mode "sexp" :toggle t)
    ("t" hl-todo-mode "todo" :toggle t))
   "Coding"
   (("p" smartparens-mode "smartparens" :toggle t)
    ("P" smartparens-strict-mode "smartparens strict" :toggle t)
    ("S" show-smartparens-mode "show smartparens" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t))
   "Emacs"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))

(pretty-hydra-define hydra-lookup
  (:title " Lookup" :color blue :quit-key "q")
  ("Documents"
   (("." devdocs-lookup "DevDocs Lookup"))))

(pretty-hydra-define hydra-projectile
  (:title " Project" :color blue :quit-key "q")
  ("Find"
   (("a"   counsel-projectile-ag)
    ("m"   projectile-multi-occur)
    ("b"   consult-projectile-switch-to-buffer))
   "File/Buffer"
    (("c"   projectile-invalidate-cache)
     ("d"   consult-projectile-find-dir)
     ("fF"  consult-projectile-find-file)
     ("ff"  projectile-find-file-dwim)
     ("fd"  projectile-find-file-in-directory)
     ("g"   ggtags-update-tags)
     ("i"   projectile-ibuffer)
     ("k"   projectile-kill-buffers)
     ("p"   projectile-switch-project)
     ("r"   consult-projectile-recentf)
     ("x"   projectile-remove-known-project)
     ("X"   projectile-cleanup-known-projects)
     ("z"   projectile-cache-current-file))))

(pretty-hydra-define hydra-org
  (:title " Org" :color blue :quit-key "q")
  ("Actions"
   (("c" org-capture "Capture")
    ("a" org-agenda "Show Agenda")
    ("rr" org-refile "Refile current entry")
    ("rd" org-refile-copy "Duplicate current entry")
    ("o" (find-file "~/org/todo.org")))
   "Download"
   (("ds" org-download-screenshot "Insert screenshot")
    ("dc" org-download-clipboard "Attach image from clipboard"))
   "Roam"
   (("n" org-roam-node-find "Find roam node"))))

;; avy
; Jump to things in tree-style
(use-package avy)
(use-package avy-zap)

(pretty-hydra-define hydra-goto
  (:title " Goto" :color blue :quit-key "q" :foreign-keys warn :separator "-")
  ("Got"
   (("j" avy-goto-char       "char")
    ("t" avy-goto-char-timer "timer")
    ("w" avy-goto-word-1     "word")
    ("r" avy-resume "resume"))
   "Line"
   (("h" avy-goto-line        "head")
    ("e" avy-goto-end-of-line "end")
    ("n" consult-goto-line    "number"))
   "Topic"
   (("o"  consult-outline      "outline")
    ("m"  consult-imenu        "imenu")
    ("gm" consult-global-imenu "global imenu"))
   "Error"
   ((","  flycheck-previous-error "previous" :exit nil)
    ("."  flycheck-next-error "next" :exit nil)
    ("l" consult-flycheck "list"))
   "Spell"
   ((">"  flyspell-goto-next-error "next" :exit nil)
    ("cc" flyspell-correct-at-point "correct" :exit nil))))

;; meow
(defun meow-setup ()
 (setq ;; meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
       meow-use-cursor-position-hack t
       meow-use-enhanced-selection-effect t)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   ;; '("j" . "H-j")
   ;; '("k" . "H-k")
   '("j" . hydra-goto/body)
   '("f" . hydra-file/body)

   '("." . point-to-register)
   '(">" . jump-to-register)
   '("p" . hydra-projectile/body)
   '("w" . hydra-window/body)
   '("v" . magit-status)
   '("l" . hydra-lsp-bridge/body)
   '("d" . hydra-lookup/body)
   '("t" . hydra-toggle/body)
   '("o" . hydra-org/body)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("=" . indent-region)
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :init
  (meow-global-mode 1)
  :config
  (setq meow-use-clipboard t)
  (meow-setup))

(global-set-key (kbd "M-,") 'previous-buffer)
(global-set-key (kbd "M-.") 'next-buffer)
(global-set-key (kbd "M-!") 'async-shell-command)
(global-set-key (kbd "M-@") 'shell-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exec Path From Shell
; Do it on window systems and Unix based environment
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; direnv
(use-package direnv
  :init
  (direnv-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package vertico
;;   :straight (:files (:defaults "extensions/*"))
;;   :custom
;;   (vertico-cycle t)
;;   :init
;;   (vertico-mode 1)
;;   :config
;;   (require 'vertico-directory))

;; (use-package marginalia
;;   :custom
;;   (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
;;   :init
;;   (marginalia-mode 1))

;; (use-package consult
;;   :straight (:files (:defaults "extensions/*"))
;;   :bind
;;   (("C-s" . consult-line)
;;    ("M-y" . consult-yank-pop)
;;    :map minibuffer-local-map
;;    ("C-r" . consult-history))
;;   :custom (consult-async-min-input 1)
;;   :config
;;   (setq completion-in-region-function #'consult-completion-in-region))

;; (use-package orderless
;;   :init
;;   ;; Configure a custom style dispatcher (see the Consult wiki)
;;   ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
;;   ;;       orderless-component-separator #'orderless-escapable-split-on-space)
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles partial-completion)))))

(use-package counsel
  :blackout
  :bind
  ("C-s" . swiper)
  :init
  (ivy-mode)
  (counsel-mode)
  :config
  (blackout 'ivy-mode)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

(use-package embark
  :bind
  ([remap describe-bindings] . embark-bindings)
  ("C-." . embark-act))

(use-package embark-consult
  :after (embark consult))

(use-package corfu
  :after vertico
  :straight (:files (:defaults "extensions/*"))
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.3))
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-scroll-margin 5)        ;; Use scroll margin
  :hook ((corfu-mode . corfu-popupinfo-mode))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("<C-return>" . corfu-insert))
  :init
  (global-corfu-mode))

(use-package cape
  :after corfu
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package tabnine-capf
  :after cape
  :commands (tabnine-capf-install-binary
             tabnine-capf-restart-server)
  :straight (:host github :repo "50ways2sayhard/tabnine-capf" :files ("*.el" "*.sh"))
  :hook (kill-emacs . tabnine-capf-kill-process)
  :config
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point))

(use-package company
  :blackout
  :init
  (setq company-minimum-prefix-length 1
        company-dabbrev-minimum-length 3
        company-dabbrev-code-time-limit 0.3
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-files-exclusions '(".git/" ".DS_Store")
        company-global-modes '(not vterm-mode)
        company-frontends '(company-pseudo-tooltip-frontend
                            ;; always show candidates in overlay tooltip
                            company-echo-metadata-frontend)
        company-backends '((company-files company-yasnippet company-capf :separate company-dabbrev-code))
        company-auto-commit nil
        company-dabbrev-other-buffers nil
        company-dabbrev-code-other-buffers nil
        company-dabbrev-ignore-case t
        company-dabbrev-code-ignore-case t
        company-dabbrev-downcase nil
        company-selection-wrap-around t
        completion-ignore-case t
        ;; Trigger completions immediately.
        company-idle-delay 0
        ;; Number the candidates (use M-1, M-2 etc to select completions).
        company-show-numbers t)
  
  (add-hook 'after-init-hook #'global-company-mode))

(use-package company-quickhelp
  :after company
  :bind
  (:map company-active-map ("C-h" . company-quickhelp-manual-begin)))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :blackout
  :hook (after-init . yas-global-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package ivy-yasnippet
  :after (ivy yasnippet))

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

(use-package doom-snippets
  :after yasnippet
  :straight (:host github :repo "hlissner/doom-snippets" :files ("*.el" "snippets")))

;; (use-package company-tabnine
;;   :init
;;   (add-to-list 'company-backends #'company-tabnine))

(use-package copilot
  :hook (prog-mode . copilot-mode)
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-;") 'copilot-accept-completion-by-word))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;; Programming ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; Linum
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; font-lock annotations like TODO in the source code
(use-package hl-todo
  :init
  (global-hl-todo-mode 1))

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

(use-package devdocs
  ;; TODO config me
  )

(use-package magit
  :init
  (setq magit-auto-revert-mode nil))

(use-package markdown-mode)
(use-package dumb-jump) ;; TODO config dumb-jump binding
(use-package terraform-mode)
(use-package nix-mode)
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker)

(use-package consult-eglot
  :after (consult eglot))

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

(use-package eglot
  :commands eglot eglot-ensure
  :hook
  (python-mode . eglot-ensure)
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

(use-package counsel-projectile
  :after (counsel projectile))

;;;; Python
(defun python-doc ()
  (interactive)
  (setq-local devdocs-current-docs '("python-3.11")))

(add-hook 'python-mode-hook #'eldoc-mode)

(when (fboundp #'devdocs-lookup)
  (add-hook 'python-mode-hook #'python-doc))

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

;; (use-package python-mode
;;   :custom
;;   (python-indent-guess-indent-offset-verbose nil))

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


;;
;; => Org
;;
(use-package org
  :ensure t
  :config
  (require 'org-tempo) ;; enable org templates; by default it's disabled
  ;; on Org > 9.2, more info:
  ;; https://emacs.stackexchange.com/a/46992

  (setq org-startup-indented t
        org-startup-folded t
        org-todo-keywords '((sequence "[ ](t)" "[*](p)" "[-](n)" "|" "[x](d)" "[c](c@)"))
        org-use-speed-commands t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-directory "~/org"
        org-agenda-files (list "~/org")
        org-log-refile t
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-refile-targets
        '((org-agenda-files . (:maxlevel . 2)))
        org-capture-templates
        '(("t" "Task Entry"        entry
           (file+headline "~/org/todo.org" "Inbox")
           "* [ ] %?
:PROPERTIES:
:Added:     %U
:END:" :empty-lines 0)
          ))

   ;; Return or left-click with mouse should follow links
  (customize-set-variable 'org-return-follows-link t)
  (customize-set-variable 'org-mouse-1-follows-link t)

  ;; Display links as the description provided
  (customize-set-variable 'org-descriptive-links t)

  ;; Hide markup markers
  (customize-set-variable 'org-hide-emphasis-markers t)

  ;; disable auto-pairing of "<" in org mode
  (add-hook 'org-mode-hook (lambda ()
                             (setq-local electric-pair-inhibit-predicate
                                         `(lambda (c)
                                            (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  (with-eval-after-load 'org
    (org-indent-mode t)
    (require 'org-id))
  )

(use-package org-appear
  :after org
  :config
  (add-hook 'org-mode-hook 'org-appear-mode)
  )

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode)
  )

(use-package org-download
  :after org
  :config
  (setq org-download-method 'directory
        org-download-heading-lvl nil
        org-download-timestamp "_%Y%m%d-%H%M%S"
        org-image-actual-width t
        org-download-screenshot-method "flameshot gui --raw > %s")

  (customize-set-variable 'org-download-image-dir "images")
  )

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
  ;; FIXME it's not working
  :mode "\\.k?sql\\'")

;; TODO add https://github.com/purcell/sqlformat

;;
;; -> TypeScript
;;
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook
  (typescript-mode . tree-sitter-hl-mode)
  :config
  (setq typescript-indent-level 2))

;;
;; -> Go
;;
(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (go-mode . tree-sitter-hl-mode)
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))
