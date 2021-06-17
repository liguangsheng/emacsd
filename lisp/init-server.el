;; init-server.el

(use-package server
  :ensure nil
  :commands (server-running-p server-start)
  :hook (after-init . (lambda () (unless (server-running-p) (server-start)))))

(provide 'init-server)
