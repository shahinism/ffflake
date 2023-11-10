;;; sh-utils.el --- Emacs Lisp utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(provide 'sh-utils)

;; Simplify reloading my configuration
(defun reload-emacs ()
  "Reload Emacs configuration."
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c r") 'reload-emacs)

;; It is the opposite of fill-paragraph
;; by: Stefan Monnier <foo at acm.org>.
(defun unfill-paragraph (&optional region)
  "Unfill the paragraph at point.
When REGION is non-nil, unfill each paragraph in the region,"
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(define-key global-map "\M-Q" 'unfill-paragraph)

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1" "cl-lib-0.5"
  :url "https://github.com/purcell/exec-path-from-shell"
  :if (memq window-system '(mac ns x))
  :disabled t    ;; FIXME doesn't support nushell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(leaf direnv
  :doc "direnv integration"
  :url "https://github.com/wbolster/emacs-direnv"
  :ensure t
  :init
  (direnv-mode))

(leaf undo-fu-session
  :doc "Persist undo history between sessions"
  :url "https://github.com/emacsmirror/undo-fu-session"
  :ensure t
  :hook
  (after-init . global-undo-fu-session-mode)
  :custom
  (undo-fu-session-linear . nil)
  (undo-fu-session-file-limit . nil)
  (undo-fu-session-incompatible-files . '("\\.gpg$"
                                        "/COMMIT_EDITMSG\\'"
                                        "/git-rebase-todo\\'"))
  :config
  (setq undo-fu-session-directory (sh/cache-dir "undo-fu-session"))
  (when (executable-find "zstd")
    (setq undo-fu-session-compression 'zstd))
  )

(leaf vundo
  :doc "Visualize undo history"
  :url "https://github.com/casouri/vundo"
  :ensure t
  :commands (vundo)
  :custom
  (vundo-compact-display . t)
  :bind
  ("C-c u" . vundo)
  (vundo-mode-map
        ("l" . vundo-forward)
        ("h" . vundo-backward)
        ("n" . vundo-next)
        ("p" . vundo-previous)
        ("q" . vundo-quit))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  )

(leaf ace-window
  :doc "Quickly switch windows"
  :url "https://github.com/abo-abo/ace-window"
  :ensure t
  :pretty-hydra
  ((:color teal :quit-key "q")
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
    (("i" text-scale-increase "in" :color amaranth)
     ("o" text-scale-decrease "out" :color amaranth)
     ("0" (lambda ()
            (interactive)
            (zoom-in/out 0)) "reset"))))
  )

(leaf zoxide
  :doc "Find file by zoxide"
  :url "https://github.com/emacsmirror/zoxide"
  :ensure t
  :bind
  ("C-c z" . zoxide-find-file)
  )

(leaf avy
  :doc "Jump to things in Emacs tree-style"
  :url "https://github.com/abo-abo/avy"
  :ensure t
  :bind
  ("C-c j" . avy/body)
  :pretty-hydra
  ((:color teal :quit-key "q")
   ("Char"
    (("c" avy-goto-char-timer "Char Timer")
     ("C" avy-goto-char "Char"))
    "Word"
    (("w" avy-goto-word-1 "Word")
     ("W" avy-goto-word-0 "Word 0"))
    "Line"
    (("l" avy-goto-line "Line")
     ("L" avy-goto-end-of-line "End of Line")
     ("m" avy-move-line "Move Line")
     ("y" avy-copy-line "Copy Line")
     ("k" avy-kill-whole-line "Kill Line"))
    "Region"
    (("M" avy-move-region "Move Region")
     ("R" avy-resume "Resume")
     ("K" avy-kill-region "Kill Region")
     ("Y" avy-copy-region "Copy Region"))
    ))
  )

(leaf restclient
  :doc "HTTP REST client tool for Emacs"
  :url "https://github.com/pashky/restclient.el"
  :ensure t
  :mode
  ("\\.http\\'" . restclient-mode)
  :bind (:restclient-mode-map ("C-c C-c" . restclient/body))
  :pretty-hydra
  ((:color teal :quit-key "q")
   ("Actions"
    (("S" restclient-http-send-current-stay-in-window "send")
     ("s" restclient-http-send-current "send and switch")
     ("R" restclient-http-send-current-raw-stay-in-window "send raw")
     ("r" restclient-http-send-current-raw "send raw and switch")
     ("E" restclient-http-send-current-raw-stay-in-window "send raw")
     ("E" restclient-http-send-current-raw "send raw and switch")
     ("c" restclient-copy-curl-command "copy curl command")
     ("C" restclient-copy-curl-command-as-org-link "copy curl command as org link"))
    "Navigation"
    (("n" restclient-jump-next "next")
     ("p" restclient-jump-prev "previous")
     ("N" restclient-narrow-to-current "narrow to current"))
    "Misc"
    (("t" restclient-mark-current "mark current")
     ("T" restclient-mark-current-and-copy "mark current and copy")
     ("u" restclient-unmark "unmark")
     ("U" restclient-unmark-all "unmark all"))
    )))
;;; sh-utils.el ends here
