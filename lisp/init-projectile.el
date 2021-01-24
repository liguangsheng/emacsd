(use-package projectile
  :init
  (setq projectile-indexing-method 'native
	projectile-cache-file  (ucache "projectile.cache")
	projectile-known-projects-file (ucache "projectile-bookmarks.eld"))
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package helm-projectile)

(use-package counsel-projectile)

(provide 'init-projectile)
