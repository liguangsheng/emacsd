;; init.el --- Emacs configuration -*- lexical-binding: t; -*- ;;; Commentary:
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
 ;; 中英文字体
 ;; https://github.com/powerline/fonts
 ;; curl -L https://github.com/hbin/top-programming-fonts/raw/master/install.sh | bash(expand-file-name "")
 en-fonts '("Inconsolata for Powerline" 14  "Source Code Pro" 12 "Menlo" 12
	    "Courier New" 12)
 cn-fonts '("华文细黑" 12 "宋体" 12 "PingFang SC" 12 "微软雅黑" 12)
 ;; 使用主题
 theme 'doom-Iosvkem
 ;; Proxy
 ;; url-proxy-services '(("http"  . "127.0.0.1:1080")
 ;; 		         ("https" . "127.0.0.1:1080")))
 server-p t
 use-tabnine nil
 )

;; -----------------------------------------------------------------------------
(require 'init-core (concat user-emacs-directory "lisp/init-core"))
(require 'init-my-functions)
(require 'init-better-defaults)
(require 'init-packages)

;; Features
(require 'init-features)
(require 'init-evil)
(require 'init-helm)
(require 'init-yasnippet)
(require 'init-company)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-treemacs)
(require 'init-server)
(require 'init-lsp)

;; Major Modes
(require 'init-org)
(require 'init-markdown)
(require 'init-protobuf)
(require 'init-yaml)
(require 'init-json)
(require 'init-python)
(require 'init-go)
(require 'init-rust)
(require 'init-cc)
(require 'init-ruby)
(require 'init-lua)
(require 'init-typescript)
(require 'init-bazel)

(require 'init-keybindings)
(require 'init-fonts)
(require 'init-themes)


;; Load custom file
(when (file-readable-p custom-file) (load custom-file))

;;; ----------------------------------------------------------------------------
;; Experimental:

;;; init.el ends here

