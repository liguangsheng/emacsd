;; init.el --- Emacs configuration -*- lexical-binding: t; -*- ;;
;;; Commentary:
;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(require 'init-core (concat user-emacs-directory "lisp/init-core"))

;;; ----------------------------------------------------------------------------
;;; 快速配置

(setq-default
 ;; 显示行号
 show-line-number-p t
 ;; Prefer fonts
 ;; https://github.com/powerline/fonts
 ;; curl -L https://github.com/hbin/top-programming-fonts/raw/master/install.sh | bash
 prefer-en-font "Go Mono for Powerline:size=14"

 ;; Proxy
 ;; url-proxy-services '(("http"  . "127.0.0.1:1080")
 ;; 		         ("https" . "127.0.0.1:1080")))
 use-tabnine *i-am-rich*
 ;; use posframe if possible
 prefer-posframe nil
 ;; use all-the-icons if possible
 prefer-icons (display-graphic-p)
 ;; 加载主题，如果存在的话
 ;; 推荐: 白天用'solo-jazz, 夜间用'wombat
 prefer-theme 'doom-dark+
 ;; python 可执行文件地址
 my/python-executable "python3"
 ;; org files directory
 org-directory "~/sync/org"
 )

(when *win64*
  (setq org-directory "e:/sync/org/"
	my/python-executable "C:\\Program Files\\Python39\\python.exe"
	))

;; -----------------------------------------------------------------------------
(require 'init-my-functions)
(require 'init-packages)
(require 'init-better-defaults)

;; Features
(require 'init-fonts)
(require 'init-themes)
(require 'init-icons)
(require 'init-evil)
(require 'init-hydra)
(require 'init-features)
(require 'init-dired)
(require 'init-ivy)
(require 'init-yasnippet)
(require 'init-company)
(require 'init-tabnine)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-treemacs)
(require 'init-which-key)
(require 'init-modeline)
(require 'init-lsp)
(require 'init-keybindings)

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
(when (and custom-file (file-readable-p custom-file) (load custom-file)))

;;; ----------------------------------------------------------------------------
;; Experimental:

;;; init.el ends here

