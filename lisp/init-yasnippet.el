(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (withf 'shut-up (shut-up (yas-global-mode 1))))

(provide 'init-yasnippet)
