;;; init.el -*- lexical-binding: t; -*-

;; DirVish
(leaf dirvish
  :ensure t
  :config
  (dirvish-override-dired-mode)
  (setq dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
 )

;; parentheses
(electric-pair-mode 1)

(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; Use settings from .editorconfig file when present
(leaf editorconfig
  :ensure t
  :global-minor-mode editorconfig-mode)

;; flyspell + UI
(leaf flyspell
  :doc "Spell checker"
  :url "https://www.emacswiki.org/emacs/FlySpell"
  :ensure t
  :hook
  (prog-mode-hook . flyspell-prog-mode)
  ((markdown-mode-hook git-commit-mode-hook) . flyspell-mode)
  :custom
  (ispell-program-name . "aspell")
  (ispell-extra-args   . '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :custom-face
  (flyspell-incorrect  . '((t (:underline (:color "#f1fa8c" :style wave)))))
  (flyspell-duplicate  . '((t (:underline (:color "#50fa7b" :style wave))))))

(leaf flyspell-correct
  :doc "Correcting misspelled words with flyspell using favourite interface"
  :url "https://github.com/d12frosted/flyspell-correct"
  :ensure t
  :bind*
  ("C-M-i" . flyspell-correct-at-point)
  :custom
  (flyspell-correct-interface . #'flyspell-correct-completing-read))

(leaf anzu
  :doc "Displays current match and total matches information"
  :url "https://github.com/emacsorphanage/anzu"
  :ensure t
  :bind ("M-r" . anzu-query-replace-regexp))

(provide 'lt-editing)
