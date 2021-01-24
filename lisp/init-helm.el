(use-package helm)

(use-package helm-icons
  :config (helm-icons-enable))

(use-package helm-themes)

(use-package helm-describe-modes
  :bind ([remap describe-mode] . #'helm-describe-modes))

(use-package helm-rg
  :config (setq helm-rg-default-directory 'git-root))

(use-package helm-swoop)

(use-package ace-jump-helm-line
  :config
  (with-eval-after-load "helm"
    (define-key helm-map (kbd "C-'") 'ace-jump-helm-line)))

(use-package helm-gitignore)

(use-package helm-xref)

(use-package helm-ls-git
  :bind (("C-x C-d" . 'helm-browse-project)))

(use-package helm-posframe
  :if prefer-posframe
  :init (setq helm-posframe-parameters
	      '((left-fringe . 10)
		(right-fringe . 10)))
  :config
  (helm-posframe-enable))

(use-package helm-company)

;; (use-package helm-lsp
;;   :config
;;   (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(provide 'init-helm)
