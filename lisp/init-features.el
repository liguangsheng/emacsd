;;; init-features.el --- Features -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dash)
(use-package shut-up)
(when prefer-posframe (use-package posframe))
(use-package winner
  :config (winner-mode 1))

(use-package restart-emacs)

(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-decrease default-text-scale-reset)
  :custom ((default-text-scale-amount 5)))

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

;; 这个feature可能会影响company的候选框的显示
;; (use-package fill-column-indicator
;;   :init (setq fci-rule-column 120)
;;   (add-hook 'prog-mode-hook #'fci-mode))

;; 扩展选择区域
(use-package expand-region)

;; 跳转
(use-package avy
  :init
  (avy-setup-default))

;; emoji
(use-package emojify
  :config (global-emojify-mode))

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

;; 高亮symbol
(use-package highlight-symbol
  :init (highlight-symbol-mode))

;; 高亮当前行
(use-package hl-line
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

(use-package vi-tilde-fringe
  :config
  (global-vi-tilde-fringe-mode))

(provide 'init-features)
