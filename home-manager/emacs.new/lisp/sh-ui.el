;;; sh-ui.el --- Emacs look -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(provide 'sh-ui)


(leaf nano
  :url "https://github.com/rougier/nano-emacs"
  :disabled t         ;; NOTE it looks beautiful, but too limiting on
                      ;; using a custom theme. For now, given the
                      ;; readability of the code, I prefer to use
                      ;; custom themes, and load only the modules I
                      ;; like.
  :straight (nano :host github :repo "rougier/nano-emacs")
  :pre-setq
  ;; (nano-font-family-monospaced . "Fira Code")
  ;; (nano-font-family-proportional . "Fira Sans")
  :require nano-base-colors
  :require nano-faces
  :require nano-help
  :require nano-writer
  :require nano-modeline
  :require nano-theme

  :config

  (defun nano-theme-set-zenburn ()
    "Apply Zenburn theme base."
    (setq frame-background-mode    'dark)
    (setq nano-color-foreground "#DCDCCC") ;; Foreground (light yellow)
    (setq nano-color-background "#3F3F3F") ;; Background (dark gray)
    (setq nano-color-highlight  "#4F4F4F") ;; Highlight (darker gray)
    (setq nano-color-critical   "#DC143C") ;; Critical (crimson red)
    (setq nano-color-salient    "#7F9F7F") ;; Salient (soft green)
    (setq nano-color-strong     "#DFAF8F") ;; Strong (light orange)
    (setq nano-color-popout     "#D78787") ;; Popout (light red)
    (setq nano-color-subtle     "#5F5F5F") ;; Subtle (gray)
    (setq nano-color-faded      "#8F8F8F") ;; Faded (light gray)

    (set-cursor-color nano-color-critical)
    ;; to allow for toggling of the themes.
    )

  (nano-theme-set-zenburn)
  (nano-theme)
  (nano-faces)
  (setq nano-theme-var "light")
  )


(leaf doom-themes
  :url "https://github.com/doomemacs/themes"
  :ensure t
  :custom
  (doom-themes-enable-bold . t)
  (doom-themes-enable-italic . t)
  :config
  (load-theme 'doom-zenburn t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  )

(leaf all-the-icons
  :url "https://github.com/domtronn/all-the-icons.el"
  :ensure t
  :if (display-graphic-p))

(leaf lambda-line
  :url "https://github.com/Lambda-Emacs/lambda-line"
  :straight (lambda-line :host github :repo "Lambda-Emacs/lambda-line")
  :custom
  ;; Having it on top, will interfer with Hydra, moving window too
  ;; much down, which makes it impossible to use.
  (lambda-line-position . 'bottom)                ;; Set position of status-line
  (lambda-line-abbrev . t)                        ;; abbreviate major modes
  (lambda-line-hspace . "  ")                     ;; add some cushion
  (lambda-line-prefix . t)                        ;; use a prefix symbol
  (lambda-line-prefix-padding . nil)              ;; no extra space for prefix
  (lambda-line-status-invert . nil)               ;; no invert colors
  (lambda-line-gui-ro-symbol . " ⨂")             ;; symbols
  (lambda-line-gui-mod-symbol . " ⬤")
  (lambda-line-gui-rw-symbol . " ◯")
  :config
  (lambda-line-mode)
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_")))
  )

;; Set font
(defun font-existsp (font)
  "Check to see if the named FONT is available."
  (if (null (x-list-fonts font))
      nil t))

;; Set default font.
(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font-13.5"))
;;; sh-ui.el ends here
