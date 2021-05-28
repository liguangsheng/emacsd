(use-package persistent-scratch
  :init (setq persistent-scratch-save-file (expand-file-name "scratch.org" org-directory))
  :config (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

(provide 'init-scratch)
