(use-package counsel
  :init (setq ivy-height 30
	      ivy-initial-inputs-alist nil))

(use-package smex)

;; (use-package all-the-icons-ivy
;;   :if prefer-icons
;;   :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :ensure t
  :if prefer-icons
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package ivy-posframe
  :if prefer-posframe
  :init (setq ivy-posframe-border-width 2
	      ivy-posframe-display-functions-alist '((complete-symbol . ivy-posframe-display-at-point)
						     (t . ivy-posframe-display)))
  :config (ivy-posframe-mode 1))

(provide 'init-ivy)
