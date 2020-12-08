;;; init-yaml.el --- Support YAML files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)))

(use-package yaml-mode
  :mode (("\\.ya?ml\\'" . yaml-mode)))

(provide 'init-yaml)
