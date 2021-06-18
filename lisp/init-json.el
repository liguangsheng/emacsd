;;; init-json.el --- Support JSON files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package json-mode
  :mode (("\\.json\\'" . json-mode))
  :hook (json-mode . lsp-deferred))

(provide 'init-json)
