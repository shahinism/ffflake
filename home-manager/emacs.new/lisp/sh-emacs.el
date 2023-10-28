;;; sh-emacs.el --- Emacs generic configuration
;;; Commentary:
;;; Code:
(provide 'sh-emacs)

;; Define the user configuration folders and ensure they exist:
(defvar sh/cache-dir (expand-file-name "cache/" user-emacs-directory)
  "Directory to store persistent cache files.")
(defvar sh/etc-dir (expand-file-name "etc/" user-emacs-directory)
  "The user's configuration etc/ folder.")
(defvar sh/var-dir (expand-file-name "var/" user-emacs-directory)
  "The user's configuration var/ folder.")

(unless (file-exists-p sh/cache-dir)
  (mkdir sh/cache-dir))

(unless (file-exists-p sh/etc-dir)
  (mkdir sh/etc-dir))

(unless (file-exists-p sh/var-dir)
  (mkdir sh/var-dir))

(defun sh/cache-dir (name)
  "Return the full path to the cache dir with NAME."
  (expand-file-name name sh/cache-dir))

;; When an error happens, show me the stack trace
(setq debug-on-error t)

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
                        (expand-file-name "recentf" sh/var-dir))

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
                        (expand-file-name "history" sh/var-dir))

;; backups
(setq make-backup-files nil
      auto-save-default nil)

;; Remember the position in the buffer
(save-place-mode +1)

;; Enable pair matching globally
(electric-pair-mode 1)

(show-paren-mode 1)
(setq show-paren-style 'mixed)

;;; sh-emacs.el ends here
