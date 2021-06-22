(use-package doom-themes :defer t)
(use-package zenburn-theme :defer t)
(use-package dracula-theme :defer t)
(use-package material-theme :defer t)
(use-package immaterial-theme :defer t)
(use-package github-modern-theme :defer t)
(use-package moe-theme
  :config (moe-theme-apply-color 'cyan))
(use-package kaolin-themes :defer t)
(use-package solo-jazz-theme :defer t)
(use-package leuven-theme :defer t)
(use-package material-theme :defer t)
(use-package apropospriate-theme :defer t)

(defvar prefer-theme 'wombat)

(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/lgs-theme")

(when (display-graphic-p)
  (add-hook 'after-init-hook
	    (lambda ()
	      (load-theme prefer-theme t)
	      (kaolin-treemacs-theme) ;; for treemacs
	      )))

(provide 'init-themes)
