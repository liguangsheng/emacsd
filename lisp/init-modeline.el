(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25
	doom-modeline-bar 10
	doom-modeline-buffer-file-name-style 'relative-to-project
	doom-modeline-icon nil
	doom-modeline-major-mode-icon t
	doom-modeline-modal-icon t
	doom-modeline-number-limit 99
	doom-modeline-lsp t)

  ;; (setq doom-modeline-height 1)
  ;; (set-face-attribute 'mode-line nil :name "Microsoft Yahei" :height 100)
  ;; (set-face-attribute 'mode-line-inactive nil :family "Microsoft Yahei" :height 100)
  ;; (custom-set-faces
  ;;  '(mode-line ((t (:family "Microsoft Yahei" :height 0.9))))
  ;;  '(mode-line-inactive ((t (:family "Microsoft Yahei" :height 0.9)))))
  )

(provide 'init-modeline)
