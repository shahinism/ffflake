;;; init.el -*- lexical-binding: t; -*-

(leaf eglot
  :ensure t
  :commands eglot eglot-ensure
  :hook
  (python-mode-hook . eglot-ensure)
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

(provide 'lt-lsp)
