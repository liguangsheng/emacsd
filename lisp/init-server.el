;; init-server.el

(setq server-socket-dir (ucache "server")
      server-auth-dir   (ucache "server")
      server-name       "server")

(defun emacs-server-exist-p ()
  (file-exists-p (expand-file-name server-name server-socket-dir)))

(defun restart-emacs-server ()
  (interactive)
  (server-force-delete)
  (server-start))

(unless (emacs-server-exist-p) (server-start))

(provide 'init-server)
