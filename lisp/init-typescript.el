;;; init-typwscript.el --- Support Typescript language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
	 ("\\.tsx\\'" . typescript-mode)))

(provide 'init-typescript)
