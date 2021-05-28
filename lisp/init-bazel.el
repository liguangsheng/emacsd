;;; init-bazel.el --- Support Bazel language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package bazel
  :mode (("WORKSPACE\\'" . bazel-mode)
	 ("BUILD\\'" . bazel-mode)))

(provide 'init-bazel)
