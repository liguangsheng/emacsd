(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  ;; :init (doom-modeline-mode)
  :config (setq doom-modeline-height 25
		doom-modeline-bar 3
		doom-modeline-buffer-file-name-style 'relative-to-project
		doom-modeline-icon t
		doom-modeline-major-mode-icon t
		))

(provide 'init-modeline)
