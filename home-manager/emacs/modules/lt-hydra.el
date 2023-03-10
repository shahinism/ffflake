;;; init.el -*- lexical-binding: t; -*-

(leaf major-mode-hydra
  :doc "Use pretty-hydra to define template easily"
  :url "https://github.com/jerrypnz/major-mode-hydra.el"
  :ensure t
  :require pretty-hydra)

;; (leaf hydra-posframe
;;   :doc "Show hidra hints on posframe"
;;   :url "https://github.com/Ladicle/hydra-posframe"
;;   :if (window-system)
;;   :el-get "Ladicle/hydra-posframe"
;;   :global-minor-mode hydra-posframe-mode
;;   :custom
;;   (hydra-posframe-border-width . 5)
;;   (hydra-posframe-parameters   . '((left-fringe . 8) (right-fringe . 8)))
;;   :custom-face
;;   (hydra-posframe-border-face . '((t (:background "#323445")))))

(leaf *hydra-search
  :doc "Search functions"
  :bind
  ("M-s" . *hydra-search/body)
  :pretty-hydra
  ((:title " Search" :color blue :quit-key "q" :foreign-keys warn :separator "-")
   ("Buffer"
    (("l" consult-line "line")
     ("o" consult-outline "outline")
     ("m" consult-imenu "imenu"))
    "Project"
    (("f" affe-find "find")
     ("r" affe-grep "grep"))
    "Document"
    (("df" consult-find-doc "find")
     ("dd" consult-grep-doc "grep")))))

(provide 'lt-hydra)
