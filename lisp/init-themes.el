(use-package doom-themes :defer t)
(use-package zenburn-theme :defer t)
(use-package dracula-theme :defer t)
(use-package badwolf-theme :defer t)
(use-package material-theme :defer t)
(use-package immaterial-theme :defer t)
(use-package github-theme :defer t)
(use-package github-modern-theme :defer t)
(use-package noctilux-theme :defer t)
(use-package firecode-theme :defer t)
(use-package moe-theme :defer t)
(use-package kaolin-themes :defer t)
(use-package solo-jazz-theme :defer t)
(use-package jazz-theme :defer t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/solo-jazz-emacs-theme")

(defvar prefer-theme 'wombat)

(when (display-graphic-p)
  (add-hook 'after-init-hook
	    (lambda ()
	      (load-theme prefer-theme t)
	      (kaolin-treemacs-theme) ;; for treemacs
	      )))

(provide 'init-themes)
