;;; init.el -*- lexical-binding: t; -*-

(leaf all-the-icons :ensure t)
(leaf elisp-demos :ensure t)

;;; Font
(defun lt/ui--set-default-font (spec)
  "Set the default font based on SPEC

SPEC is expected to be a plist with the same key names as
accepted by `set-default-attribute'."
  (when spec
    (apply 'set-face-attribute 'default nil spec)))

;; TODO set font only when availabe
(lt/ui--set-default-font '(:font "FiraCode Nerd Font" :weight regular :height 100))

;;; Modeline
(leaf doom-modeline
  :ensure t
  :config

  (add-hook 'after-init-hook #'doom-modeline-mode)

  ;; Configure `doom-modeline'
  (customize-set-variable 'doom-modeline-height 15)
  (customize-set-variable 'doom-modeline-bar 6)
  (customize-set-variable 'doom-modeline-minor-modes t)
  (customize-set-variable 'doom-modeline-buffer-file-name-style 'truncate-except-project)
  )

;;;; Help Buffers

;; Make `describe-*' screens more helpful
(leaf helpful
  :ensure t
  :config
  (global-set-key [remap describe-command] #'helpful-command)
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-key] #'helpful-key)
  (global-set-key [remap describe-symbol] #'helpful-symbol)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key (kbd "C-h F") #'helpful-function)
  
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)

  ;; Bind extra `describe-*' commands
  (global-set-key (kbd "C-h K") #'describe-keymap)
  )

;; Theme
;; (leaf doom-themes
 ;;  :config
 ;;  (setq doom-themes-enable-bold t
 ;;        doom-themes-enable-italic t)

 ;;  (load-theme 'doom-zenburn t)
;;  )
(load-theme 'modus-vivendi t)


;; which key help
(leaf which-key
  :ensure t
  :config
  (which-key-mode)
  )

(provide 'lt-ui)
