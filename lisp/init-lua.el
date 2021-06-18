;;; Lua: 
(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :interpreter ("lua" . lua-mode)
  :hook (lua-mode . lsp-deferred))

(provide 'init-lua)
