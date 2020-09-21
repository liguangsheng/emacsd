;;; init-protobuf.el --- Support proto files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package protobuf-mode
  :mode (("\\.proto\\'" . protobuf-mode)))

(provide 'init-protobuf)
