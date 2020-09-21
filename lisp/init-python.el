;;; init-python.el --- Support Python language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python-mode
  :init
  (add-hook 'python-mode 'lsp)
  (when (executable-find "python3")
    (setq python-shell-interpreter "python3"))
  (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/")))

(provide 'init-python)
