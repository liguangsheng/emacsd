;;; init-features.el --- Features -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode))
(use-package dash)
(use-package shut-up)
(use-package posframe
  :if prefer-posframe)
(use-package winner
  :config (winner-mode 1))

(use-package restart-emacs)

(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-decrease default-text-scale-reset)
  :custom ((default-text-scale-amount 5)))

;; 智能括号
;; (use-package smartparens
;;   :config
;;   (sp-with-modes
;;       '(c++-mode objc-mode c-mode go-mode)
;;     (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
;;     (sp-local-pair "(" nil :post-handlers '(:add ("||\n[i]" "RET"))))
;;   (when smartparens-p (smartparens-global-mode)))

;; 平滑滚动屏幕
(use-package good-scroll
  :config
  (good-scroll-mode 1))

;; 这个feature可能会影响company的候选框的显示
(use-package fill-column-indicator
  :disabled 
  :init (setq fci-rule-column 120)
  (add-hook 'prog-mode-hook #'fci-mode))

;; 扩展选择区域
(use-package expand-region)

;; 跳转
(use-package avy
  :init
  (avy-setup-default))

;; emoji
(when (display-graphic-p)
  (use-package emojify
    :hook (after-init . global-emojify-mode)))

;; 彩虹分隔符
(use-package rainbow-delimiters
  :init (hook-gross-modes #'rainbow-delimiters-mode))

;; 高亮缩进
(use-package highlight-indentation
  :disabled
  :init (hook-gross-modes #'highlight-indentation-current-column-mode))

;; 高亮数字
(use-package highlight-numbers
  :config (hook-gross-modes #'highlight-numbers-mode))

;; 高亮TODO
(use-package hl-todo
  :init (global-hl-todo-mode))

(use-package volatile-highlights
  :config (volatile-highlights-mode t))

;; 高亮对应的paren
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
	      show-paren-when-point-in-periphery t))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package vi-tilde-fringe
  :init
  (global-vi-tilde-fringe-mode))

(use-package symbol-overlay
  :diminish
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all)
         ([M-f3] . symbol-overlay-remove-all))
  )

(provide 'init-features)
