;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;;; ----------------------------------------------------------------------------
;;; Quick Settings

(setq-default
 ;; 显示行号
 show-line-number-p t
 ;; 启动时窗口最大化
 maximize-frame-at-start-p t
 ;; 平滑滚动
 smooth-scrolling-p t
 ;; Prefer fonts
 ;; https://github.com/powerline/fonts
 ;; curl -L https://github.com/hbin/top-programming-fonts/raw/master/install.sh | bash(expand-file-name "")
 prefer-fonts '((;; windows font setting
		 :if (eq system-type 'windows-nt)
		 :font-name "Go Mono for Powerline"
		 :font-size 12
		 :chinese-font-name "WenQuanYi Micro Hei"
		 :chinese-font-size 14
		 ))
 ;; Proxy
 ;; url-proxy-services '(("http"  . "127.0.0.1:1080")
 ;; 		         ("https" . "127.0.0.1:1080")))
 use-tabnine nil
 ;; use posframe if possible
 prefer-posframe t
 ;; use all-the-icons if possible
 prefer-icons t
 ;; available hydra evil-leader
 prefer-leader 'hydra
 )

;; -----------------------------------------------------------------------------
(require 'init-core (concat user-emacs-directory "lisp/init-core"))
(require 'init-my-functions)
(require 'init-better-defaults)
(require 'init-packages)

;; Features
(require 'init-benchmark-init)
(require 'init-features)
(require 'init-icons)
(require 'init-evil)
(require 'init-hydra)
(require 'init-yasnippet)
(require 'init-company)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-treemacs)
(require 'init-server)
(require 'init-which-key)
(require 'init-modeline)
(require 'init-lsp)
(require 'init-helm)
(require 'init-ivy)
(require 'init-keybindings)
(require 'init-fonts)
(require 'init-themes)

;; Major Modes
(require 'init-bazel)
(require 'init-cc)
(require 'init-eshell)
(require 'init-go)
(require 'init-json)
(require 'init-lua)
(require 'init-markdown)
(require 'init-org)
(require 'init-protobuf)
(require 'init-python)
(require 'init-ruby)
(require 'init-rust)
(require 'init-typescript)
(require 'init-yaml)
(require 'init-toml)
(require 'init-powershell)
(require 'init-graphql)

;; Load custom file
(when (file-readable-p custom-file) (load custom-file))

;;; ----------------------------------------------------------------------------
;; Experimental:

;;; init.el ends here

