(setq toggle-eshell--last-buffer "*scratch*")

(defun toggle-eshell-project-root ()
  (interactive)
  (if (string-prefix-p "*eshell" (buffer-name)) (switch-to-buffer toggle-eshell--last-buffer)
      (progn
      (setq toggle-eshell--last-buffer (buffer-name))
      (message (format "switch to eshell from %s" (buffer-name)))
      (projectile-run-eshell nil))))

(use-package projectile
  :bind (("<f7>" . toggle-eshell-project-root))
  :init
  (setq projectile-indexing-method 'native
	projectile-cache-file  (ucache "projectile.cache")
	projectile-known-projects-file (ucache "projectile-bookmarks.eld"))
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(provide 'init-projectile)
