;;; LSP:
(use-package lsp-mode
  :diminish lsp-mode
  :bind (("M-b" . xref-find-definitions)
	 ("M-]" . xref-find-definitions)
	 ("M-[" . xref-pop-marker-stack))
  :init
  (setq lsp-auto-guess-root t       ; Detect project root
	lsp-prefer-flymake nil      ; Use lsp-ui and flycheck
	flymake-fringe-indicator-position 'right-fringe
	flymake-diagnostic-functions '(lsp--flymake-backend nil)
	lsp-inhibit-message t
	lsp-message-project-root-warning t
	create-lockfiles nil)

  :config
  ;; Restart server/workspace in case the lsp server exits unexpectedly.
  ;; https://emacs-china.org/t/topic/6392
  (defun restart-lsp-server ()
    "Restart LSP server."
    (interactive)
    (lsp-restart-workspace)
    (revert-buffer t t)
    (message "LSP server restarted."))

  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
    (setq lsp-diagnostics-modeline-scope :project)
    (add-hook 'lsp-managed-mode-hook 'lsp-diagnostics-modeline-mode))

  (use-package lsp-treemacs)
  )

(use-package company-lsp
  :init (setq company-lsp-cache-candidates 'auto))

(use-package lsp-ui
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq scroll-margin 0))

(provide 'init-lsp)
