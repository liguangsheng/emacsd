;;; init-toml.el --- Support TOML files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)))

(provide 'init-toml)
