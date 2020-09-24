(use-package counsel
  :init (setq ivy-height 30)
  :bind (("M-x"   . 'counsel-M-x)
	 ("C-x b" . 'counsel-switch-buffer)
	 ("C-h v" . 'counsel-describe-variable)
	 ("C-h f" . 'counsel-describe-function)
	 ("C-x c b" . 'ivy-resume)
	 ("C-s"   . 'swiper))
  :config
  (with-eval-after-load "evil-leader"
    (evil-leader/set-key
      "mm" 'counsel-buffer-or-recentf
      "bb" 'counsel-switch-buffer
      "ff" 'counsel-find-file
      "fr" 'counsel-recentf
      "hy" 'counsel-yank-pop
      )))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package counsel-projectile
  :config
  (with-eval-after-load "evil-leader"
    (evil-leader/set-key
      "P"   'counsel-projectile
      "pd"  'counsel-projectile-find-dir
      "pf"  'counsel-projectile-find-file
      "pp"  'counsel-projectile-switch-project
      "psg" 'counsel-projectile-grep
      "psr" 'counsel-projectile-rg
      "pss" 'counsel-projectile-ag
      )))

(provide 'init-ivy)
