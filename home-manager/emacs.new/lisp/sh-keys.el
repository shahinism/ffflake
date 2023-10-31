;;; sh-keys.el --- Emacs Keys -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(provide 'sh-keys)

(leaf which-key
  :url "https://github.com/justbur/emacs-which-key"
  :ensure t
  :custom
  (which-key-idle-delay . 0.3)
  :config
  (which-key-mode)
  ;; NOTE it's also possible to use ~(which-key-setup-minibuffer)~
  ;; however, on a minibuffer command, you'll end up with a minibuffer
  ;; with the size of 1/3 of the frame.
  (which-key-setup-side-window-bottom))

(leaf hydra
  :url "https://github.com/abo-abo/hydra"
  :ensure t)

(defun meow-setup ()
 (setq ;; meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
       meow-use-cursor-position-hack t
       meow-use-enhanced-selection-effect t)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   ;; '("j" . "H-j")
   ;; '("k" . "H-k")
   '("j" . hydra-goto/body)
   '("f" . hydra-file/body)

   '("z" . hydra-chat/body)
   '("." . point-to-register)
   '(">" . jump-to-register)
   '("p" . project/body)
   '("w" . ace-window/body)
   '("v" . magit-status)
   '("l" . hydra-lsp-bridge/body)
   '("t" . hydra-toggle/body)
   '("o" . hydra-org/body)
   '("b" . consult-buffer)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("=" . indent-region)
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))


(leaf meow
  :url "https://github.com/meow-edit/meow"
  :ensure t
  :custom
  (meow-use-clipboard . t)
  :init
  (meow-global-mode 1)
  :hook
  (after-init-hook . meow-setup)
  (git-commit-setup . meow-insert-mode)
  ;; (org-capture-mode . meow-insert-mode)   ;; FIXME it kills lambda line
  )

(global-set-key (kbd "M-,") 'previous-buffer)
(global-set-key (kbd "M-.") 'next-buffer)
(global-set-key (kbd "M-!") 'async-shell-command)
(global-set-key (kbd "M-@") 'shell-command)


;;; sh-keys.el ends here
