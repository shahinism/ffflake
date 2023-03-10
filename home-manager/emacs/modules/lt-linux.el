;;; init.el -*- lexical-binding: t; -*-

(leaf exec-path-from-shell
  :ensure t
  :config

  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
)

(provide 'lt-linux)
