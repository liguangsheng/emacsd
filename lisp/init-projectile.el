(use-package projectile
  :init
  (setq projectile-indexing-method 'native)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile)

(provide 'init-projectile)
