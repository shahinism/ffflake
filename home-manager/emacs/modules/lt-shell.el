;;; init.el -*- lexical-binding: t; -*-


;; Functions
(defun lt/shell-has-sudo-p ()
  "Check if the current command on eshell prompt, starts with sudo."
  (let ((command-string (buffer-substring-no-properties
                         (progn (eshell-bol) (point)) (point-max))))
    (string-match-p "^sudo " command-string)))

(defun lt/shell-sudo-add ()
  (progn
    (eshell-bol)
    (insert "sudo ")))

(defun lt/shell-sudo-pop ()
  (progn
    (eshell-bol)
    (while (re-search-forward "sudo " nil t)
      (replace-match "" t nil))))

(defun lt/shell-sudo-toggle ()
  "Toggle sudo with the current command"
  (interactive
   (save-excursion
       (if (lt/shell-has-sudo-p)
           (lt/shell-sudo-pop)
         (lt/shell-sudo-add)))
     ))

(defun lt/eshell-pop-show (name)
  "Create a pop up window with eshell named NAME."
  (let* ((window (split-window
                  (frame-root-window)
                  '45
                  'below))
         (buffer (get-buffer name)))

    (select-window window)
    (if buffer
        (set-window-buffer window name)
      (progn
        (eshell window)
        (rename-buffer name)))
    ))

(defun lt/eshell-pop-hide (name)
  "Hide the existing pop up window with eshell named NAME."
  (let ((shell-buffer (get-buffer-window name)))
    (select-window shell-buffer)
    (bury-buffer)
    (delete-window)))

(defun lt/eshell-pop-toggle ()
  "Toggle eshell pop up window."
  (interactive)
  (let ((name "shell-buffer"))
    (if (get-buffer-window name)
        (lt/eshell-pop-hide name)
      (lt/eshell-pop-show name))
    ))

;; Configuration
(leaf eshell-z
  :ensure t
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (require 'eshell-z)))
)

(leaf esh-help
  :ensure t
  :config
  (setup-esh-help-eldoc)
  )

(leaf eshell-did-you-mean
  :ensure t
  :config
  (eshell-did-you-mean-setup)
)

(leaf eshell-syntax-highlighting
  :ensure t
  :config
  (add-hook 'eshell-mode-hook 'eshell-syntax-highlighting-mode)
  )

;; Bindings
(defun is-evil-p ()
  "Check wheter we are in evil state or not."
  (or (eq evil-state nil) (eq evil-state 'emacs)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (if (is-evil-p)
                (local-set-key (kbd "C-r") 'consult-history)
              (progn
                (evil-local-set-key 'insert (kbd "C-r") 'consult-history)
                (evil-local-set-key 'normal (kbd "C-r") 'consult-history))
              )))

;; Vterm
(leaf vterm
  :ensure t
  :commands vterm-mode
  :hook (vterm-mode-hook . hide-mode-line-mode)
  :preface
  ;; HACK Because vterm clusmily forces vterm-module.so's compilation on us when
  ;;      the package is loaded, this is necessary to prevent it when
  ;;      byte-compiling this file (`leaf' blocks eagerly loads packages
  ;;      when compiled).
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 5000)
  )

(leaf vterm-toggle
  :ensure t
  :bind
  (:vterm-mode-map
   ;; you can cd to the directory where your previous buffer file exists
   ;; after you have toggle to the vterm buffer with `vterm-toggle'.
   ([(control return)] . vterm-toggle-insert-cd)
   ("s-n" . vterm-toggle-forward)     ; Switch to next vterm buffer
   ("s-p" . vterm-toggle-backward))   ; Switch to previous vterm buffer
  :config
  (setq vterm-toggle-fullscreen-p nil)

  ;; Show buffer in bottom
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  )

(leaf hide-mode-line :ensure t)

(provide 'lt-shell)
