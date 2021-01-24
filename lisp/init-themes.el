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
(use-package apropospriate-theme :defer t)
(use-package moe-theme :defer t)
(use-package kaolin-themes :defer t)

;; (defun final-theme ()
;;   (cond
;;    ((eq theme nil) default) 
;;    ((or (eq theme 'random) (string-equal theme "random")) (random-theme))
;;    (t theme)))

;; (defun load-theme-dwim ()
;;   (interactive)
;;   (let ((final-theme (final-theme)))
;;     (load-theme final-theme t)
;;     (message (format "load theme: %s" (symbol-name final-theme)))))

;; (unless (eq theme 'default)
;;   (load-theme-dwim))

(add-hook 'after-init-hook
	  (lambda ()
	    (load-theme 'kaolin-galaxy t)
	    (kaolin-treemacs-theme)
	    ))

(provide 'init-themes)
