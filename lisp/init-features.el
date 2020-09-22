;;; init-features.el --- Features -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package shut-up)

(use-package restart-emacs
  :bind (("C-c q r" . restart-emacs))
  :init
  (with-eval-after-load "evil-leader"
    (evil-leader/set-key "qr" 'restart-emacs)))

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

(use-package fill-column-indicator
  :init (setq fci-rule-column 120)
  (add-hook 'prog-mode-hook #'fci-mode))

;; 扩展选择区域
(use-package expand-region
  :bind (("C-c e p" . er/mark-inside-pairs)
	 ("C-c e q" . er/mark-inside-quotes)
	 ("C-c e u" . er/mark-url)
	 ("C-c e e" . er/mark-email)
	 ("C-c e a" . er/mark-text-paragraph)
	 ("C-c e v" . er/expand-region)
	 ("C-c v"   . er/expand-region))
  :init
  (with-eval-after-load "evil-leader"
    (evil-leader/set-key
      "ep" 'er/mark-inside-pairs
      "eq" 'er/mark-inside-quotes
      "eu" 'er/mark-url
      "ee" 'er/mark-email
      "ea" 'er/mark-text-paragraph
      "ev" 'er/expand-region
      "v" 'er/expand-region
      )))

;; 跳转
(use-package avy
  :bind (("C-c SPC" . avy-goto-word-1)
	 ("C-c l"   . avy-goto-line))
  :init
  (avy-setup-default)
  (with-eval-after-load "evil-leader"
    (evil-leader/set-key
      "SPC" 'avy-goto-word-1
      "l"   'avy-goto-line
      )))

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

(use-package vi-tilde-fringe
  :config
  (global-vi-tilde-fringe-mode))

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

(provide 'init-features)
