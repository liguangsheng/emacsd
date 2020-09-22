(use-package rg)
(use-package ripgrep)
(use-package projectile
  :init
  (setq projectile-indexing-method 'native
	projectile-cache-file  (ucache "projectile.cache")
	projectile-known-projects-file (ucache "projectile-bookmarks.eld"))
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package helm-projectile
  :init
  (evil-leader/set-key
    "pd" 'helm-projectile-find-dir
    "pf" 'helm-projectile-find-file-dwim
    "psr" 'helm-projectile-rg
    "pp" 'helm-projectile
    "pr" 'helm-projectile-recentf
    ))

(provide 'init-projectile)
