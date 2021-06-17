;; treemacs
(use-package treemacs
  :init
  (setq treemacs-width 40
	treemacs-python-executable my/python-executable)
  )

  ;; :bind
  ;; (([f8] . my/treemacs-select-window))

  ;; :config
  ;; (dolist (tface '(treemacs-file-face
  ;; 		   treemacs-directory-face
  ;; 		   treemacs-git-modified-face
  ;; 		   treemacs-git-added-face
  ;; 		   treemacs-git-renamed-face
  ;; 		   treemacs-git-untracked-face
  ;; 		   treemacs-git-unmodified-face
  ;; 		   treemacs-git-conflict-face
  ;; 		   treemacs-git-ignored-face
  ;; 		   ))
  ;;   (let ((font "Microsoft YaHei UI"))
  ;;     (if (font-exists-p font)
  ;; 	  (set-face-attribute tface nil
  ;; 			      :font (format "%s:pixelsize=%d" font 12)
  ;; 			      :height 90
  ;; 			      :weight 'normal)
  ;; 	(set-face-attribute tface nil :height 90)
  ;; 	)))

  ;; (treemacs-follow-mode t)
  ;; (treemacs-filewatch-mode t)
  ;; (treemacs-fringe-indicator-mode t)
  ;; (pcase (cons (not (null (executable-find "git")))
  ;; 	       (not (null treemacs-python-executable)))
  ;;   (`(t . t)
  ;;    (treemacs-git-mode 'deferred))
  ;;   (`(t . _)
  ;;    (treemacs-git-mode 'simple)))


;; (use-package treemacs-evil)

;; (use-package treemacs-projectile
;;   :after treemacs projectile
;;   :ensure t)

;; (use-package treemacs-icons-dired
  ;; :after treemacs dired
  ;; :ensure t
  ;; :config (treemacs-icons-dired-mode))

;; (use-package treemacs-magit
;;   :after treemacs magit
;;   :ensure t)

;; (use-package treemacs-persp ;;treemacs-persective if you use perspective.el vs. persp-mode
;;   :after treemacs persp-mode ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

(provide 'init-treemacs)
