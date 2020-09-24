(use-package helm
  :bind (("M-x" . 'helm-M-x)
	 ("C-x b" . 'helm-mini))

  :init
  (customize-set-variable 'helm-ff-lynx-style-map t)
  (customize-set-variable 'helm-imenu-lynx-style-map t)
  (customize-set-variable 'helm-semantic-lynx-style-map t)
  (customize-set-variable 'helm-occur-use-ioccur-style-keys t)
  (customize-set-variable 'helm-grep-use-ioccur-style-keys t)

  (evil-leader/set-key
    "bb" 'helm-mini
    "ff" 'helm-find-files
    "fr" 'helm-recentf
    "hi" 'helm-imenu
    "hy" 'helm-show-kill-ring
    ))

(use-package helm-icons
  :config (helm-icons-enable))

(use-package helm-themes)

(use-package helm-describe-modes
  :bind ([remap describe-mode] . #'helm-describe-modes))

(use-package helm-rg
  :config (setq helm-rg-default-directory 'git-root))

(use-package helm-swoop
  :bind (("C-s" . 'helm-swoop)))

(use-package helm-projectile
  :init
  (evil-leader/set-key
    "P"   'helm-projectile
    "pd"  'helm-projectile-find-dir
    "pf"  'helm-projectile-find-file-dwim
    "pp"  'helm-projectile-switch-project
    "pr"  'helm-projectile-recentf
    "pb"  'helm-projectile-switch-to-buffer
    "psg" 'helm-projectile-grep
    "psr" 'helm-projectile-rg
    "pss" 'helm-projectile-ag
    "pxe" 'helm-projectile-switch-to-eshell
    ))

(use-package ace-jump-helm-line
  :config
  (with-eval-after-load "helm"
    (define-key helm-map (kbd "C-'") 'ace-jump-helm-line)))

(use-package helm-gitignore)

(use-package helm-xref)

(use-package helm-ls-git
  :bind (("C-x C-d" . 'helm-browse-project)))

(provide 'init-helm)
