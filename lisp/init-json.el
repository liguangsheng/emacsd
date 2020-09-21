;;; init-json.el --- Support JSON files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)))

(provide 'init-json)
