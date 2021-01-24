(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
	company-tooltip-limit 24            ; bigger popup window
	company-idle-delay .2               ; decrease delay before autocompletion popup shows
	company-echo-delay 0                ; remove annoying blinking
	company-minimum-prefix-length 2
	company-require-match nil
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil)
  ;; Nicer looking faces
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil))))))

(use-package company-quickhelp
  :config (company-quickhelp-mode 1))

(when use-tabnine
  (use-package company-tabnine))

(when prefer-posframe
  (use-package company-posframe
    :config
    (company-posframe-mode 1)))

(provide 'init-company)
