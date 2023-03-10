;;; init.el -*- lexical-binding: t; -*-

; Configuration
;; Org
(leaf org
  :ensure t
  :config
  (require 'org-tempo) ;; enable org templates; by default it's disabled
  ;; on Org > 9.2, more info:
  ;; https://emacs.stackexchange.com/a/46992

  (setq org-startup-indented t
        org-startup-folded t
        org-todo-keywords '((sequence "[ ](t)" "[*](p)" "[-](n)" "|" "[x](d)" "[c](c@)"))
        org-use-speed-commands t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-directory "~/org"
        org-agenda-files (list "~/org")
        org-log-refile t
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-refile-targets
        '((org-agenda-files . (:maxlevel . 2)))
        org-capture-templates
        '(("t" "Task Entry"        entry
           (file+headline "~/org/todo.org" "Inbox")
           "* [ ] %?
:PROPERTIES:
:Added:     %U
:END:" :empty-lines 0)
          ))

  ;; Return or left-click with mouse should follow links
  (customize-set-variable 'org-return-follows-link t)
  (customize-set-variable 'org-mouse-1-follows-link t)

  ;; Display links as the description provided
  (customize-set-variable 'org-descriptive-links t)

  ;; Hide markup markers
  (customize-set-variable 'org-hide-emphasis-markers t)

  ;; disable auto-pairing of "<" in org mode
  (add-hook 'org-mode-hook (lambda ()
                             (setq-local electric-pair-inhibit-predicate
                                         `(lambda (c)
                                            (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  (with-eval-after-load 'org
    (org-indent-mode t)
    (require 'org-id))
  )

(leaf org-appear
  :url "https://github.com/awth13/org-appear"
  :doc "Make invisible parts of Org elements appear visible"
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'org-appear-mode)
  )

(leaf org-bullets
  :doc "Fancy bullets"
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode)
)

;; Org-roam
(leaf org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-v2-ack t
        org-roam-directory (file-truename org-directory))

  (unless (f-directory? org-roam-directory)
    (make-directory org-roam-directory t))
  
  :config
  (require 'org-roam)
  (org-roam-setup)
  )

;; Deft
(leaf deft
  :ensure t
  :config
  (setq deft-directory org-directory
        deft-recursive t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-use-filename-as-title t)
  )

(leaf org-download
  :url "https://github.com/abo-abo/org-download"
  :ensure t
  :config
  (setq org-download-method 'directory
        org-download-heading-lvl nil
        org-download-timestamp "_%Y%m%d-%H%M%S"
        org-image-actual-width t
        org-download-screenshot-method "flameshot gui --raw > %s")

  (customize-set-variable 'org-download-image-dir "images")
  )

;; FIXME
;; (use-package org-auto-tangle
;;   :url "https://github.com/yilkalargaw/org-auto-tangle"
;;   :defer t
;;   :hook (org-mode . org-auto-tangle-mode))

(provide 'lt-note)
