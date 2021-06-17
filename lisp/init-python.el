;;; init-python.el --- Support Python language -*- lexical-binding: t -*-
;;; Commentary:
;;     pip install 'python-language-server[all]'
;;; Code:

(use-package python-mode
  :init
  (when (executable-find "python3")
    (setq python-shell-interpreter my/python-executable))
  (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))

  :hook (python-mode .(lambda ()
			(eldoc-mode 0))))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp-deferred

(provide 'init-python)
