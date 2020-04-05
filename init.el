;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
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
 ;; 中英文字体
 ;; https://github.com/powerline/fonts
 ;; curl -L https://github.com/hbin/top-programming-fonts/raw/master/install.sh | bash
 en-fonts '("Fira Mono for Powerline" 13 "Source Code Pro" 13 "Courier New" 13)
 cn-fonts '("华文细黑" 16 "宋体" 15 "微软雅黑" 15)
 ;; 使用主题
 theme 'doom-one
 ;; Proxy
 ;; url-proxy-services '(("http"  . "127.0.0.1:1080")
 ;; 		      ("https" . "127.0.0.1:1080")))
 )

;; ----------------------------------------------------------------------------
;;; Basic
(require 'cl-lib)

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs-root-dir (file-truename user-emacs-directory)
  "Path to .emacs.d directory.")

(defconst emacs-lisp-dir  (expand-file-name "lisp/" emacs-root-dir)
  "Path to .emacs.d/lisp directory where init files exists.")

(defconst emacs-site-lisp-dir (expand-file-name "site-lisp/" emacs-root-dir)
  "Path to .emacs.d/site-lisp directory.")

;; Add dir to load-path
(add-to-list 'load-path emacs-lisp-dir)
(add-to-list 'load-path emacs-site-lisp-dir)

;; Recursive add site-lisp to load-path
(let ((default-directory emacs-site-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

;;; -----------------------------------------------------------------------
;;; My Functions

(defun indent-whole-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun move-to-front (list x)
  (cons x (remove x list)))

(defun kill-all-buffers (KILL-STARRED-BUFFER)
  "Kill all buffers."
  (dolist (buffer (buffer-list))
    (let ((bname (string-trim (buffer-name buffer))))
      (if (and (not KILL-STARRED-BUFFER)
	       (string-prefix-p "*" bname)
	       (string-suffix-p "*" bname))
	  nil
	(kill-buffer-if-not-modified buffer)
	))))

(defun kill-all-buffers-i ()
  (interactive)
  (kill-all-buffers nil))

(defun switch-to-modified-buffer ()
  "Switch to modified buffer"
  (interactive)
  (let ((buf-list (seq-filter (lambda (x)
				(not
				 (or
				  (not (buffer-modified-p x))
				  (s-prefix? "*" (buffer-name x))
				  (s-prefix? " *" (buffer-name x))
				  (s-suffix? "-mode" (buffer-name x)))))
			      (buffer-list))))
    (if buf-list
	(switch-to-buffer (first buf-list))
      (message "No buffer modified."))))

(defun open-init-el ()
  "Open ~/.emacs.d/init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-inbox ()
  "Open ~/INBOX"
  (interactive)
  (find-file "~/INBOX"))

(defun switch-to-scratch ()
  "Swtich to *scratch* buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun random-choice (LIST)
  "Get a random choice from LIST"
  (nth (mod (random) (length LIST)) LIST))

(defun random-theme ()
  "Return a random theme symbol"
  (random-choice (custom-available-themes)))

;;; ------------------------------------------------------------------------
;;; Defaults

;; Initlize Frame
(when (not (>= emacs-major-version 27))
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(defalias 'yes-or-no-p 'y-or-n-p)
(cua-mode 1)
(horizontal-scroll-bar-mode -1)
(global-auto-revert-mode 1)
(recentf-mode 1)
(ignore-errors (savehist-mode 1))
(save-place-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

;; Show line number
(when show-line-number-p
  (add-hook 'prog-mode-hook        #'display-line-numbers-mode)
  (add-hook 'fundamental-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook        #'display-line-numbers-mode))

;; Maximize frame at start
(defvar maximize-frame-at-start-p t "Maximize-frame-at-start-p.")
(when maximize-frame-at-start-p (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

;; Use utf-8 as default coding system.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))               ; pretty
(prefer-coding-system                   'utf-8)  ; pretty
(set-terminal-coding-system             'utf-8)  ; pretty
(set-keyboard-coding-system             'utf-8)  ; pretty
(set-selection-coding-system            'utf-8)  ; pretty
(setq locale-coding-system              'utf-8)  ; pretty
(setq-default buffer-file-coding-system 'utf-8)  ; with sugar on top

;; Chinese encoding for windows
(when (eq system-type 'windows-nt)
  (set-next-selection-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le))

;; Better variables
(setq
 apropos                      t
 backup-by-copying            t
 comint-prompt-read-only      t
 compilation-always-kill      t
 compilation-ask-about-save   nil
 compilation-scroll-output    t
 debug-on-error               t
 delete-old-versions          t
 history-length               1024
 idle-update-delay            0.5
 inhibit-startup-message      t
 kept-new-versions            6
 kept-old-versions            2
 large-file-warning-threshold 100000000
 vc-follow-symlinks           t
 version-control              t
 visible-bell                 0
 backup-directory-alist       `(("." . ,(concat user-emacs-directory "backups")))
 )

(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 1 1024 1024 1024 8)))) ;; 1GB

;;; ----------------------------------------------------------------------------
;;; Package Manager

;; package.el
(require 'package)
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
			 ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
			 ("melpa-stable" . "https://mirrors.ustc.edu.cn/elpa/melpa-stable/")
			 ("org" . "https://mirrors.ustc.edu.cn/elpa/org/")))

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; Initialization benchmark
(use-package benchmark-init
  :commands (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate)
  :init (benchmark-init/activate))

;; A modern Packages Menu
(use-package paradox
  :init
  (setq paradox-execute-asynchronously t
	paradox-github-token t
	paradox-display-star-count nil)

  ;; Replace default `list-packages'
  (defun my-paradox-enable (&rest _)
    "Enable paradox, overriding the default package-menu."
    (paradox-enable))
  (advice-add #'list-packages :before #'my-paradox-enable)
  :config
  (when (fboundp 'page-break-lines-mode)
    (add-hook 'paradox-after-execute-functions
	      (lambda (&rest _)
		(let ((buf (get-buffer-create "*Paradox Report*"))
		      (inhibit-read-only t))
		  (with-current-buffer buf
		    (page-break-lines-mode 1))))
	      t)))

;; Auto update packages
(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
	auto-package-update-hide-results t)
  (defalias 'upgrade-packages #'auto-package-update-now))

;;; ----------------------------------------------------------------------------
;;; Init Font
(defvar en-fonts '("Source Code Pro" 13 "Courier New" 13))
(defvar cn-fonts '("宋体" 15 "微软雅黑" 15))

(defun init/exists-p (font-name)
  "Check if font exists."
  (if (null (x-list-fonts font-name)) nil t))

(defun init/use-en (font-name font-size)
  "Set font for english."
  (set-face-attribute 'default nil
		      :font (format "%s:pixelsize=%d" font-name font-size)
		      :weight 'normal))

(defun init/use-cn (font-name font-size)
  "Set font for chinese."
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
		      (font-spec :family font-name :size font-size))))

(defun init/use-list (font-list font-func)
  "Search font in font-list, use it if exists."
  (unless (null font-list)
    (let ((font-name (car font-list))
	  (font-size (cadr font-list)))
      (if (init/exists-p font-name)
	  (funcall font-func font-name font-size)
	(init/use-list (cddr font-list) font-func)))))

(defun init/use-en-list (font-list)
  (init/use-list font-list 'init/use-en))

(defun init/use-cn-list (font-list)
  (init/use-list font-list 'init/use-cn))


(defun init-font ()
  (when (display-graphic-p)
    (init/use-en-list en-fonts)
    (init/use-cn-list cn-fonts)))

(init-font)

;;; ----------------------------------------------------------------------------
;;; Theme

(defvar theme 'default)
(use-package doom-themes :defer t)
(use-package zenburn-theme :defer t)
(use-package dracula-theme :defer t)
(use-package badwolf-theme :defer t)
(use-package material-theme :defer t)
(use-package immaterial-theme :defer t)
(use-package github-theme :defer t)
(use-package github-modern-theme :defer t)
(use-package noctilux-theme :defer t)
(use-package firecode-theme :defer t)
(use-package apropospriate-theme :defer t)
(use-package moe-theme :defer t)

(defun final-theme ()
  (cond
   ((eq theme nil) default) 
   ((or (eq theme 'random) (string-equal theme "random")) (random-theme))
   (t theme)))

(defun load-theme-dwim ()
  (interactive)
  (let ((final-theme (final-theme)))
    (load-theme final-theme t)
    (message (format "load theme: %s" (symbol-name final-theme)))))

(unless (eq theme 'default)
  (load-theme-dwim))

;;;; ----------------------------------------------------------------------------
;;;; Initilize Packages
(use-package evil
  :config
  (evil-ex-define-cmd "q"    'kill-this-buffer)
  (evil-ex-define-cmd "quit" 'evil-quit)
  (add-hook 'prog-mode-hook        #'evil-mode)
  (add-hook 'fundamental-mode-hook #'evil-mode)
  (add-hook 'text-mode-hook        #'evil-mode))

(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode)  )

(use-package evil-surround
  :config (global-evil-surround-mode 1))

;; helm
(use-package helm
  :bind (("M-x" . 'helm-M-x)
	 ("C-x b" . 'helm-mini))
  :init
  (customize-set-variable 'helm-ff-lynx-style-map t)
  (customize-set-variable 'helm-imenu-lynx-style-map t)
  (customize-set-variable 'helm-semantic-lynx-style-map t)
  (customize-set-variable 'helm-occur-use-ioccur-style-keys t)
  (customize-set-variable 'helm-grep-use-ioccur-style-keys t)
  (evil-leader/set-key
    "bb" 'helm-mini
    "ff" 'helm-find-files
    "fr" 'helm-recentf
    "hi" 'helm-imenu
    "hr" 'helm-recentf
    "hk" 'helm-show-kill-ring
    ))

(use-package helm-themes)

(use-package helm-describe-modes
  :bind ([remap describe-mode] . #'helm-describe-modes))

(use-package restart-emacs
  :init
  (evil-leader/set-key "qr" 'restart-emacs))

;; 智能括号
(defvar smartparens-p nil)
(use-package smartparens
  :config
  (sp-with-modes
   '(c++-mode objc-mode c-mode go-mode)
   (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
   (sp-local-pair "(" nil :post-handlers '(:add ("||\n[i]" "RET"))))
  (when smartparens-p (smartparens-global-mode)))

;; 平滑滚动屏幕
(defvar smooth-scrolling-p nil)
(use-package smooth-scrolling
  :init
  (setq
   smooth-scroll-margin 1
   smooth-scroll-strict-margins t)
  :config
  (when smooth-scrolling-p (smooth-scrolling-mode 1)))

;; 扩展选择区域
(use-package expand-region
  :init
  (evil-leader/set-key
    "ep" 'er/mark-inside-pairs
    "eq" 'er/mark-inside-quotes
    "eu" 'er/mark-url
    "ee" 'er/mark-email
    "ea" 'er/mark-text-paragraph
    "ev" 'er/expand-region
    "v" 'er/expand-region
    ))

;; 跳转
(use-package avy
  :init
  (evil-leader/set-key
    "SPC" 'avy-goto-word-1
    "l" 'avy-goto-line)
  :config (avy-setup-default))

;; 彩虹分隔符
(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; 高亮缩进
(use-package highlight-indentation
  :disabled
  :config (add-hook 'prog-mode-hook #'highlight-indentation-current-column-mode))

;; 高亮数字
(use-package highlight-numbers
  :config (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; 高亮TODO
(use-package hl-todo
  :config (global-hl-todo-mode))

;; 高亮symbol
(use-package highlight-symbol
  :config (highlight-symbol-mode))

;; 高亮当前行
(use-package hl-line
  :ensure nil
  :custom-face (hl-line ((t (:extend t)))) ; FIXME: compatible with 27
  :hook (after-init . global-hl-line-mode))

;; 高亮对应的paren
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
	      show-paren-when-point-in-periphery t)
  :config
  (with-no-warnings
    (defun display-line-overlay (pos str &optional face)
      "Display line at POS as STR with FACE.
FACE defaults to inheriting from default and highlight."
      (let ((ol (save-excursion
		  (goto-char pos)
		  (make-overlay (line-beginning-position)
				(line-end-position)))))
	(overlay-put ol 'display str)
	(overlay-put ol 'face
		     (or face '(:inherit highlight)))
	ol))

    (defvar-local show-paren--off-screen-overlay nil)
    (defun show-paren-off-screen (&rest _args)
      "Display matching line for off-screen paren."
      (when (overlayp show-paren--off-screen-overlay)
	(delete-overlay show-paren--off-screen-overlay))
      ;; check if it's appropriate to show match info,
      (when (and (overlay-buffer show-paren--overlay)
		 (not (or cursor-in-echo-area
			  executing-kbd-macro
			  noninteractive
			  (minibufferp)
			  this-command))
		 (and (not (bobp))
		      (memq (char-syntax (char-before)) '(?\) ?\$)))
		 (= 1 (logand 1 (- (point)
				   (save-excursion
				     (forward-char -1)
				     (skip-syntax-backward "/\\")
				     (point))))))
	;; rebind `minibuffer-message' called by
	;; `blink-matching-open' to handle the overlay display
	(cl-letf (((symbol-function #'minibuffer-message)
		   (lambda (msg &rest args)
		     (let ((msg (apply #'format-message msg args)))
		       (setq show-paren--off-screen-overlay
			     (display-line-overlay
			      (window-start) msg ))))))
	  (blink-matching-open))))
    (advice-add #'show-paren-function :after #'show-paren-off-screen)))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config (setq doom-modeline-height 25
		doom-modeline-bar 3
		doom-modeline-buffer-file-name-style 'relative-to-project
		doom-modeline-icon t
		doom-modeline-major-mode-icon t
		))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package company
  :defer nil
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
	company-tooltip-limit 24            ; bigger popup window
	company-idle-delay .2               ; decrease delay before autocompletion popup shows
	company-echo-delay 0                ; remove annoying blinking
	company-minimum-prefix-length 2
	company-require-match nil
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; Nicer looking faces
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil))))))

(use-package helm-company)

(use-package helm-rg)

(use-package projectile)

(use-package helm-projectile
  :init
  (evil-leader/set-key
    "pd" 'helm-projectile-find-dir
    "pf" 'helm-projectile-find-file-dwim
    "pg" 'helm-projectile-rg
    "pp" 'helm-projectile
    "pr" 'helm-projectile-recentf
    "ps" 'projectile-run-eshell
    ))

(use-package flycheck)

;; treemacs
(use-package treemacs
  :init
  (treemacs-resize-icons 12)
  (defun treemacs-switch-window ()
    (interactive)
    (if (treemacs-is-treemacs-window-selected?)
	(aw-flip-window)
      (progn
	(aw--push-window (selected-window))
	(treemacs-select-window))))

  (evil-leader/set-key
    "-"  'treemacs-switch-window
    "="  'helm-treemacs-workspace
    "tr" 'treemacs
    "tt" 'treemacs-switch-window
    "tw" 'treemacs-switch-workspace
    "tp" 'treemacs-add-and-display-current-project
    "ta" 'treemacs-find-tag
    )
  (when (fboundp 'doom-themes-treemacs-config)
    (doom-themes-treemacs-config))
  (use-package treemacs-projectile)
  (use-package treemacs-evil)
  )

(setq helm--treemacs-last-candidate "Default")

(defun helm--treemacs-workspace-candidates ()
  (move-to-front
   (cl-loop for ws in (treemacs-workspaces) collect (treemacs-workspace->name ws))
   helm--treemacs-last-candidate))


(defun treemacs-find-workspace (name)
  (seq-find
   (lambda (x) (string-equal name (treemacs-workspace->name x)))
   (treemacs-workspaces)))

(defun treemacs-select-workspace (ws)
  (setf (treemacs-current-workspace) ws)
  (treemacs--invalidate-buffer-project-cache)
  (treemacs--rerender-after-workspace-change)
  (run-hooks 'treemacs-switch-workspace-hook))

(defun treemacs-select-workspace-by-name (name)
  (treemacs-select-workspace (treemacs-find-workspace name))
  (message "treemacs select workspace: %s" name))

(defun helm-treemacs-workspace ()
  (interactive)
  (helm :sources (helm-build-sync-source "Helm-Treemacs"
					 :candidates (helm--treemacs-workspace-candidates)
					 :fuzzy-match t
					 :action (lambda (candidate)
						   (setq helm--treemacs-last-candidate (treemacs-workspace->name (treemacs-current-workspace)))
						   (treemacs-select-workspace-by-name candidate))
					 )
	:buffer "*helm treemacs*"))


;; server
(defvar server-p nil
  "Do you want start a emacs server")

(defun emacs-server-exist-p ()
  (file-exist-p (server-socket-path)))

(defun restart-emacs-server ()
  (interactive)
  (server-force-delete)
  (server-start))

;; (when server-p
;;   (restart-emacs-server))

;; lsp
(use-package lsp-mode
  :diminish lsp-mode
  :hook (prog-mode . lsp)
  :bind (("s-b" . xref-find-definitions)
	 ("s-]" . xref-find-definitions)
	 ("s-[" . evil-jump-backward))
  :init
  (setq lsp-auto-guess-root t)       ; Detect project root
  (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-diagnostic-functions '(lsp--flymake-backend nil))
  :config
  (setq lsp-inhibit-message t
	lsp-message-project-root-warning t
	create-lockfiles nil
	)

  ;; Restart server/workspace in case the lsp server exits unexpectedly.
  ;; https://emacs-china.org/t/topic/6392
  (defun restart-lsp-server ()
    "Restart LSP server."
    (interactive)
    (lsp-restart-workspace)
    (revert-buffer t t)
    (message "LSP server restarted."))

  (require 'lsp-clients)

  ;; C/C++ Mode
  ;; brew install ccls
  (use-package ccls
    :defines projectile-project-root-files-top-down-recurring
    :hook ((c-mode c++-mode objc-mode cuda-mode) .
	   (lambda () (require 'ccls) (lsp)))
    :init
    (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
    :config
    (with-eval-after-load 'projectile
      (setq projectile-project-root-files-top-down-recurring
	    (append '("compile_commands.json"
		      ".ccls")
		    projectile-project-root-files-top-down-recurring))))

  ;; python-mode
  (add-hook 'python-mode 'lsp)
  (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
  )


(use-package company-lsp
  :init (setq company-lsp-cache-candidates 'auto))

(use-package lsp-ui
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq scroll-margin 0))

;; org-mode
(use-package org-bullets
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; protobuf
(use-package protobuf-mode
  :mode (("\\.proto\\'" . protobuf-mode)))

;; toml
(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)))

;; yaml
(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)))

;; json
(use-package json-mode
  :mode (("\\.json\\'" . json-mode)))

;; go
(use-package go-mode
  :init
  ;; Copy system environment variables
  (when (memq window-system '(mac ns x))
    (dolist (var '("GOPATH" "GO15VENDOREXPERIMENT"))
      (unless (getenv var)
	(exec-path-from-shell-copy-env var))))
  :hook (go-mode . go-mode-hook-func)
  :bind (:map go-mode-map
	      ("C-c d d" . godef-describe)
	      ("C-c d p" . godoc-at-point)
	      ("C-c r u" . go-remove-unused-imports))
  :config
  ;; 寻找goretuens作为格式化工具
  ;; go get -u -v github.com/sqs/goreturns
  (when (executable-find "goreturns")
    (setq gofmt-command "goreturns"))

  (defun go-mode-hook-func ()
    ;; 保存buffer之前格式化文件
    (add-hook 'before-save-hook 'gofmt-before-save t)

    ;; Go support for lsp-mode using Sourcegraph's Go Language Server
    ;; go get -u -v github.com/sourcegraph/go-langserver
    (require 'lsp-go)

    ;; eyes and hands comfort
    (subword-mode 1)
    (setq tab-width 4)
    (setq indent-tabs-mode 1)

    (evil-leader/set-key
      "mdd" 'godef-describe
      "mdp" 'godoc-at-point
      "mru" 'go-remove-unused-imports)

    (bind-key "s-]" 'godef-jump go-mode-map)
    (bind-key "s-[" 'pop-tag-mark go-mode-map)))

(use-package flycheck-golangci-lint
  :if (executable-find "golangci-lint")
  :after flycheck
  :defines flycheck-disabled-checkers
  :hook (go-mode . (lambda ()
		     "Enable golangci-lint."
		     (setq flycheck-disabled-checkers '(go-gofmt
							go-golint
							go-vet
							go-build
							go-test
							go-errcheck))
		     (flycheck-golangci-lint-setup))))

(use-package go-tag
  :bind (:map go-mode-map
	      ("C-c t" . go-tag-add)
	      ("C-c T" . go-tag-remove))
  :config (setq go-tag-args (list "-transform" "camelcase")))

(use-package gotest
  :bind (:map go-mode-map
	      ("C-c a" . go-test-current-project)
	      ("C-c m" . go-test-current-file)
	      ("C-c ." . go-test-current-test)
	      ("C-c x" . go-run)))

(use-package go-gen-test
  :bind (:map go-mode-map
	      ("C-c C-t" . go-gen-test-dwim)))

;; (use-package go-eldoc
;;   :hook (go-mode . go-eldoc-setup))

(use-package go-guru
  :bind (:map go-mode-map
	      ([remap xref-find-definitions] . go-guru-definition)
	      ([remap xref-find-references] . go-guru-referrers)))

(with-eval-after-load 'company
  (use-package company-go
    :defines company-backends
    :init (cl-pushnew 'company-go company-backends)))

(with-eval-after-load 'projectile
  (use-package go-projectile
    :commands (go-projectile-mode go-projectile-switch-project)
    :hook ((go-mode . go-projectile-mode)
	   (projectile-after-switch-project . go-projectile-switch-project))))

(use-package go-add-tags)
(use-package go-dlv)
(use-package go-fill-struct)
(use-package go-impl)
(use-package go-playground)
(use-package go-rename)
(use-package go-snippets)
(use-package golint)
(use-package govet)

;; rust
(use-package rust-mode
  :init (setq rust-format-on-save t))

(use-package rust-playground)

;; lua
(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :interpreter ("lua" . lua-mode))

;; typescript
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)))

;; bazel
(use-package bazel-mode
  :mode (("WORKSPACE\\'" . bazel-mode)
	 ("BUILD\\'" . bazel-mode)))

(use-package which-key
  :init
  (setq which-key-popup-type 'side-window
	which-key-side-window-location 'bottom
	which-key-idle-delay 0.4
	which-key-separator " → "
	which-key-prefix-prefix "+"
	which-key-side-window-max-heght 0.25)
  :config
  (which-key-mode 1))

;;; ----------------------------------------------------------------------------
;;; Keybindings

(when (fboundp 'which-key-add-key-based-replacements)
  (which-key-add-key-based-replacements
    "SPC b" "buffer"
    "SPC c" "comment"
    "SPC e" "expand"
    "SPC f" "file"
    "SPC h" "helm"
    "SPC m" "mode"
    "SPC p" "projectile"
    "SPC q" "quit"
    "SPC t" "treemacs"
    "SPC w" "window"
    ))

(when (fboundp 'bind-keys)
  (bind-keys
   ("≈"       . helm-M-x)
   ("C-j"     . ace-window)
   ("C-x C-f" . helm-find-files)
   ("M-s-l"   . indent-whole-buffer)
   ("s-/"     . comment-line))

  (bind-keys
   :map evil-normal-state-map
   ("J"  . evil-scroll-page-down)
   ("K"  . evil-scroll-page-up)
   ("u"  . undo-tree-undo)
   ("U"  . undo-tree-redo)
   ("gj" . evil-join))

  (bind-keys
   :map evil-insert-state-map
   ("C-e" . move-end-of-line)
   ("C-a" . move-beginning-of-line)
   ))

(when (fboundp 'evil-leader/set-key)
  (evil-leader/set-key
    ;; file
    "fw"	'save-buffer
    "fe"	'open-init-el
    "fs"	'save-buffer

    ;; buffer
    "bd"	'kill-this-buffer
    "bD"	'kill-all-buffers-i
    "bs"	'switch-to-scratch
    "bi"	'open-inbox
    "bm"	'switch-to-modified-buffer

    ;; window
    "wd"	'delete-window
    "wn"	'other-window
    "wo"	'delete-other-windows
    "w-"	'split-window-below
    "w|"	'split-window-right
    "ww"	'ace-window
    "w SPC"	'ace-window

    ;; comment
    "cl"	'comment-line
    "cc"	'comment-dwim

    ;; quit
    "qq"	'save-buffers-kill-emacs
    ))

;; Load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-readable-p custom-file) (load custom-file))

;;; init.el ends here

