;; server
(defvar server-p nil
  "Do you want start a emacs server")

(setq server-socket-dir (ucache "server")
      server-auth-dir   (ucache "server")
      server-name       "server")

(defun emacs-server-exist-p ()
  (file-exist-p (server-socket-path)))

(defun restart-emacs-server ()
  (interactive)
  (server-force-delete)
  (server-start))

(when server-p
  (restart-emacs-server))

(provide 'init-server)
