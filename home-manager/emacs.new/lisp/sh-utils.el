;;; sh-utils.el --- Emacs Lisp utilities
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

;;; sh-utils.el ends here
