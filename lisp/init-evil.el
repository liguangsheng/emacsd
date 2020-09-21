(use-package evil
  :init
  (hook-gross-modes #'evil-mode)
  :config
  (evil-ex-define-cmd "q"    'kill-this-buffer)
  (evil-ex-define-cmd "quit" 'evil-quit))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(provide 'init-evil)
