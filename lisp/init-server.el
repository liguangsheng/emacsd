;; init-server.el

(defun emacs-server-exist-p ()
  (file-exists-p (expand-file-name (concat "server/" server-name) user-emacs-directory)))

(defun restart-emacs-server ()
  (interactive)
  (server-force-delete)
  (server-start))

(unless (emacs-server-exist-p) (server-start))

(provide 'init-server)
