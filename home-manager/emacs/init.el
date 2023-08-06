;;; init.el -*- lexical-binding: t; -*-

(unless (and (fboundp 'server-running-p)
             (server-running-p))
  (server-start))

(defvar sh-cache-dir (expand-file-name "cache/" user-emacs-directory)
  "Directory to store persistent cache files.")

(unless (file-exists-p sh-cache-dir)
  (make-directory sh-cache-dir))

(defun sh-cache-dir (name)
  "Return the full path to the cache dir with NAME."
  (expand-file-name name sh-cache-dir))

;; Add lisp folder to the load path
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'config)
(require 'sh-completion)
(require 'sh-development)
(require 'sh-org)
