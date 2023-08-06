;;; sh-org.el --- Personal Completion Configuration -*- lexical-binding: t; -*-

(provide 'sh-org)

(use-package org
  :ensure t
  :hook
  ;; disable auto-pairing of "<" in org mode
  (org-mode . (lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
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
  ;; disable auto-pairing of "<" in org mode
  (customize-set-variable 'org-hide-emphasis-markers t)

  (with-eval-after-load 'org
    (org-indent-mode t)
    (require 'org-id))
  )

(use-package org-appear
  :after org
  :config
  (add-hook 'org-mode-hook 'org-appear-mode)
  )

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode)
  )

(use-package org-download
  :after org
  :config
  (setq org-download-method 'directory
        org-download-heading-lvl nil
        org-download-timestamp "_%Y%m%d-%H%M%S"
        org-image-actual-width t
        org-download-screenshot-method "flameshot gui --raw > %s")

  (customize-set-variable 'org-download-image-dir "images")
  )

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-completion-everywhere t)
  :config
  (org-roam-setup))

(use-package org-modern
  :after org
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda-mode))
