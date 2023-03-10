;;; init.el -*- lexical-binding: t; -*-

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

;; Helper commands
(leaf crux :ensure t)

;; GPG configuration
(leaf pinentry
  :ensure t
  :config
  (pinentry-start))

(leaf keychain-environment
  :ensure t
  :config
  (keychain-refresh-environment))

(provide 'lt-default)
