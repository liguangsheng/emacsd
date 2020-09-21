;;; init-yaml.el --- Support YAML files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)))

(provide 'init-yaml)
