;;; init.el -*- lexical-binding: t; -*-

(leaf ivy :ensure t
  :config
  (global-set-key "\C-s" 'swiper))
(leaf ace-window :ensure t)
(leaf windmove :ensure t)
(leaf projectile
  :ensure t
  :config
  (projectile-mode +1))

;; Consult doesn't include `ag` support yet
(leaf counsel-projectile :ensure t)
(leaf consult-projectile :ensure t)

(leaf hydra
  :ensure t
  :config
  (defhydra hydra-toggle (:color blue)
    ("t" vterm-toggle "VTerm Toggle")
    ("T" vterm-toggle-cd "VTerm Toggle (Current Directory)"))

  (defhydra hydra-lookup (:color blue)
    ("." devdocs-lookup "DevDocs Lookup"))

  (defhydra hydra-window ()
    "
Movement^^        ^Split^         ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←       	_/_ vertical            	_h_ X←
_j_ ↓        	_-_ horizontal	        	_j_ X↓
_k_ ↑        	_z_ undo      	_a_ce 1		_k_ X↑
_l_ →        	_Z_ reset      	_s_wap		_l_ X→
_F_ollow		_D_lt Other   	_S_ave		max_i_mize
_SPC_ cancel	_o_nly this   	_d_elete	
"
    ("h" windmove-left )
    ("j" windmove-down )
    ("k" windmove-up )
    ("l" windmove-right )
    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)
    ("b" helm-mini)
    ("f" helm-find-files)
    ("F" follow-mode)
    ("a" (lambda ()
           (interactive)
           (ace-window 1)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body))
     )
    ("/" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right))
     )
    ("-" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down))
     )
    ("s" (lambda ()
           (interactive)
           (ace-window 4)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)))
    ("S" save-buffer)
    ("D" delete-window)
    ("d" (lambda ()
           (interactive)
           (ace-window 16)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body))
     )
    ("o" delete-other-windows)
    ("i" ace-maximize-window)
    ("z" (progn
           (winner-undo)
           (setq this-command 'winner-undo))
     )
    ("Z" winner-redo)
    ("SPC" nil)
    ("q" nil)
    )

  (defhydra hydra-projectile (:color teal
                              :hint nil)
    "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
 _fF_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur       _k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
    ("a"   counsel-projectile-ag)
    ("b"   consult-projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("d"   consult-projectile-find-dir)
    ("fF"  consult-projectile-find-file)
    ("ff"  projectile-find-file-dwim)
    ("fd"  projectile-find-file-in-directory)
    ("g"   ggtags-update-tags)
    ("i"   projectile-ibuffer)
    ("k"   projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("o"   projectile-multi-occur)
    ("p"   projectile-switch-project)
    ("r"   consult-projectile-recentf)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file)
    ("q"   nil "cancel" :color blue))

  (defhydra hydra-org (:color blue
                       :hint nil)
    ("c" org-capture "Capture")
    ("n" org-roam-node-find "Find roam node")
    ("a" org-agenda "Show Agenda")
    ("r" org-refile "Refile current entry")
    ("d" org-refile-copy "Duplicate current entry")
    ("o" (find-file "~/org/todo.org"))))

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
   '("j" . "H-j")
   '("k" . "H-k")

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

(leaf meow
  :ensure t
  :global-minor-mode meow-global-mode
  :config
  (setq meow-use-clipboard t)
  (meow-setup))

(leaf *global-bindings
  :init
  (define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
  :bind
  (;; Move Buffer
   ("M-," . previous-buffer)
   ("M-." . next-buffer)
   ;; Shell Command
   ("M-!" . async-shell-command)
   ("M-@" . shell-command)))

;; Avy
(leaf avy
  :doc "Jump to things in tree-style"
  :url "https://github.com/abo-abo/avy"
  :ensure t)

(leaf avy-zap
  :doc "Zap to char using avy"
  :url "https://github.com/cute-jumper/avy-zap"
  :ensure t)

(leaf *hydra-goto
  :doc "Search and move cursor"
  :bind ("M-j" . *hydra-goto/body)
  :pretty-hydra
  ((:title " Goto" :color blue :quit-key "q" :foreign-keys warn :separator "-")
   ("Got"
    (("i" avy-goto-char       "char")
     ("t" avy-goto-char-timer "timer")
     ("w" avy-goto-word-2     "word")
     ("j" avy-resume "resume"))
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
     ("cc" flyspell-correct-at-point "correct" :exit nil)))))

;; FIXME it jumps for a tab
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)

(provide 'lt-meow)

