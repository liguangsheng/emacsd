;;; init-go.el --- Support Go language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . go-mode-hook-func))

  :bind (:map go-mode-map
	      ("C-c d d" . godef-describe)
	      ("C-c d p" . godoc-at-point)
	      ("C-c r u" . go-remove-unused-imports)
	      ("C-M-l" . gofmt))
  :init
  ;; Copy system environment variables
  (when (memq window-system '(mac ns x))
    (dolist (var '("GOPATH" "GO15VENDOREXPERIMENT"))
      (unless (getenv var)
	(exec-path-from-shell-copy-env var))))

  (defun go-mode-hook-func ()
    (setq tab-width 4
	  indent-tabs-mode 1)
    (subword-mode 1)
    (lsp-deferred)
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  )

(use-package flycheck-golangci-lint
  :if (executable-find "golangci-lint")
  :after flycheck
  :defines flycheck-disabled-checkers
  :hook (go-mode . (lambda ()
		     "Enable golangci-lint."
		     (setq flycheck-disabled-checkers '(go-gofmt
							go-golint
							go-vet
							go-build
							go-test
							go-errcheck))
		     (flycheck-golangci-lint-setup))))

(use-package go-tag
  :bind (:map go-mode-map
	      ("C-c t" . go-tag-add)
	      ("C-c T" . go-tag-remove))
  :config (setq go-tag-args (list "-transform" "camelcase")))

(use-package gotest
  :bind (:map go-mode-map
	      ("C-c a" . go-test-current-project)
	      ("C-c m" . go-test-current-file)
	      ("C-c ." . go-test-current-test)
	      ("C-c x" . go-run)))

(use-package go-gen-test
  :bind (:map go-mode-map
	      ("C-c C-t" . go-gen-test-dwim)))

;; use lsp instead
;; (use-package go-guru
;;   :bind (:map go-mode-map
;; 	      ([remap xref-find-definitions] . go-guru-definition)
;; 	      ([remap xref-find-references] . go-guru-referrers)))

(use-package go-projectile
  :after projectile
  :commands (go-projectile-mode go-projectile-switch-project)
  :hook ((go-mode . go-projectile-mode)
	 (projectile-after-switch-project . go-projectile-switch-project)))

(use-package go-add-tags)
(use-package go-dlv)
(use-package go-fill-struct)
(use-package go-impl)
(use-package go-playground)
(use-package go-rename)
(use-package go-snippets)
(use-package golint)
(use-package govet)

(use-package flycheck-gometalinter
  :init
  (setq flycheck-gometalinter-vendor t)
  :config
  (progn
    (flycheck-gometalinter-setup)))

(provide 'init-go)
