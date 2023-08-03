;;; init.el -*- lexical-binding: t; -*-

(unless (and (fboundp 'server-running-p)
             (server-running-p))
  (server-start))

;; Add lisp folder to the load path
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'config)
(require 'sh-completion)
(require 'sh-development)
