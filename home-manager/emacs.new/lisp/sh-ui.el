;;; sh-ui.el --- Emacs look -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(provide 'sh-ui)


(leaf nano
  :url "https://github.com/rougier/nano-emacs"
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

;; Set font
(defun font-existsp (font)
  "Check to see if the named FONT is available."
  (if (null (x-list-fonts font))
      nil t))

;; Set default font. First one found is selected.
(cond
 ((eq window-system nil) nil)
 ((font-existsp "FiraCode Nerd Font")
  (set-face-attribute 'default nil :height 138 :font "FiraCode Nerd Font"))
 ((font-existsp "Hack Nerd Font")
  (set-face-attribute 'default nil :height 138 :font "Hack Nerd Font"))
 )

;;; sh-ui.el ends here
