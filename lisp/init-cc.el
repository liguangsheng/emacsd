;;; init-cc.el --- Support C/C++ -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; C/C++:
;; Installation:
;;   brew install ccls
(use-package ccls
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
	 (lambda () (require 'ccls) (lsp)))
  :init
  (setq ccls-initialization-options '(:index (:comments 2)
					     :completion (:detailedLabel t)))
  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
	  (append '("compile_commands.json"
		    ".ccls")
		  projectile-project-root-files-top-down-recurring))))


(provide 'init-cc)
