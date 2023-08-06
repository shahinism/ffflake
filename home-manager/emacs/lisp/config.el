;;; init.el -*- lexical-binding: t; -*-

(provide 'config)

;; Define the user configuration var and etc folders and ensure they
;; exist:
(defvar lt/config-etc-dir (expand-file-name "etc/" user-emacs-directory)
  "The user's configuration etc/ folder")
(defvar lt/config-var-dir (expand-file-name "var/" user-emacs-directory)
  "The user's configuration var/ folder")

(unless (file-exists-p lt/config-etc-dir)
  (mkdir lt/config-etc-dir))

(unless (file-exists-p lt/config-var-dir)
  (mkdir lt/config-var-dir))

;;;;;;;;;;;;;;;;;;;;;;;;; Defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set default coding system
(set-default-coding-systems 'utf-8)

;; Visually flash instead of beep!
(customize-set-variable 'visible-bell 1)

;; Change to around ~100 MB
(customize-set-variable 'large-file-warning-threshold (* 100 1000 1000))

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

;; https://github.com/jwiegley/use-package/issues/436
(require 'bind-key)
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tools ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; => Handy
;;
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)


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
;; NOTE requires to run `M-x nerd-fonts-install-fonts'
(use-package doom-modeline
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar 6)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  :init
  (doom-modeline-mode 1))

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

(pretty-hydra-define hydra-chat
  (:title " Chat-GPT" :color blue :quit-key "q")
  ("Shell"
   (("s" chatgpt-shell "Start shell"))
  "Code"
   (("d" chatgpt-shell-describe-code "Describe code")
    ("r" chatgpt-shell-refactor-code "Refactor code")
    ("u" chatgpt-shell-generate-unit-test "Generate Unit Tests")))
   )

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

(pretty-hydra-define hydra-projectile
  (:title " Project" :color blue :quit-key "q")
  ("Find File"
   (("fF"  consult-projectile-find-file)
    ("ff"  projectile-find-file-dwim)
    ("fd"  consult-projectile-find-dir)
    ("r"   consult-projectile-recentf)
    ("d"   projectile-find-dir))
   "Search/Tags"
   (("a"   consult-ag)
    ("g"   ggtags-update-tags)
    ("o"   projectile-multi-occur))
   "Buffers"
   (("i"   projectile-ibuffer)
    ("b"   projectile-switch-to-buffer)
    ("k"   projectile-kill-buffers))
   "Cache"
   (("C"   projectile-invalidate-cache)
    ("z"   projectile-cache-current-file))
   "Project"
   (("t"   eat-project)
    ("p"   projectile-switch-project)
    ("s"   projectile-switch-project)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("c"   projectile-compile-project))))

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
   (("nl" org-roam-buffer-toggle "Toggle roam buffer")
    ("ni" org-roam-node-insert "Insert roam node")
    ("nf" org-roam-node-find "Find roam node")
    ("ng" org-roam-graph "Show roam graph")
    ("nc" org-roam-capture "Capture roam node")
    ("nt" org-roam-dailies-find-today "Find today's roam node")
    ("nr" org-roam-dailies-find-tomorrow "Find tomorrow's roam node")
    ("np" org-roam-dailies-find-yesterday "Find yesterday's roam node")
    ("nj" org-roam-dailies-capture-today "Capture today's roam node")
    ("na" org-roam-alias-add "Add roam alias")
    ("nd" org-roam-alias-delete "Delete roam alias"))))

;; avy
; Jump to things in tree-style
(use-package avy)
(use-package avy-zap)

(pretty-hydra-define hydra-goto
  (:title " Goto" :color blue :quit-key "q" :foreign-keys warn :separator "-")
  ("Got"
   (("c" avy-goto-char       "char")
    ("j" avy-goto-char-timer "timer")
    ("f" avy-goto-word-1     "word")
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
    ("'" flyspell-correct-at-point "correct" :exit nil))))

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

   '("z" . hydra-chat/body)
   '("." . point-to-register)
   '(">" . jump-to-register)
   '("p" . hydra-projectile/body)
   '("w" . hydra-window/body)
   '("v" . magit-status)
   '("l" . hydra-lsp-bridge/body)
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
  :hook
  (git-commit-setup . meow-insert-mode)
  (org-capture-mode . meow-insert-mode)
  (eat--semi-char-mode . meow-insert-mode)
  (meow-insert-exit . meow--corfu-quit)
  :config
  (setq meow-use-clipboard t)
  (meow-setup)

  (defun meow--corfu-quit ()
    (when corfu--candidates
      (corfu-quit))
    )
 )

(global-set-key (kbd "M-,") 'previous-buffer)
(global-set-key (kbd "M-.") 'next-buffer)
(global-set-key (kbd "M-!") 'async-shell-command)
(global-set-key (kbd "M-@") 'shell-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exec Path From Shell
; Do it on window systems and Unix based environment
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; direnv
(use-package direnv
  :init
  (direnv-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Programming ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Python
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

