;; Initlize Frame
;; After emacs 27.0, use `early-init-file' initlize frame
;; Before emacs 27.0, use this
(when (< emacs-major-version 27)
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
  (hook-gross-modes #'display-line-numbers-mode))

;; Maximize frame at start
(defvar maximize-frame-at-start-p t "Maximize-frame-at-start-p.")
(when maximize-frame-at-start-p (add-to-list 'initial-frame-alist
					     '(fullscreen . maximized)))

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

;; Set bigger line spacing and vertically-center the text
(defun set-bigger-spacing ()
  (setq-local default-text-properties '(line-spacing 0.20 line-height 1.20)))
(hook-gross-modes #'set-bigger-spacing)

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
 font-lock-maximum-size       5000000
 )

;; Change default path
(setq
 custom-file                    (ucache "custom.el")
 desktop-dirname                (ucache "desktop")
 backup-directory-alist         `(("." . ,(ucache "backups")))
 recentf-save-file              (ucache "recentf")
 auto-save-file-name-transforms `((".*" ,(ucache "auto-save-list") t))
 save-place-file                (ucache "places")
 savehist-file                  (ucache "savehist")
 )

(provide 'init-better-defaults)
