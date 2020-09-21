;; helm
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
    "hr" 'helm-recentf
    "hk" 'helm-show-kill-ring
    ))

(use-package helm-themes)

(use-package helm-describe-modes
  :bind ([remap describe-mode] . #'helm-describe-modes))

(use-package helm-rg)

(provide 'init-helm)
