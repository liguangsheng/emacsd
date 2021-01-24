(use-package hydra)

(use-package hydra-posframe
  :if prefer-posframe
  :load-path "~/.emacs.d/site-lisp/hydra-posframe"
  :hook (after-init . hydra-posframe-mode)
  :init
  (setq hydra-posframe-border-width 2
	hydra-posframe-poshandler 'posframe-poshandler-frame-bottom-center
	hydra-posframe-parameters '((left-fringe . 5)(right-fringe . 5)))
  ;; :custom-face (hydra-posframe-border-face ((t (:background "#bf616a"))))
  ;; :custom-face (hydra-posframe-face ((t (:background "#3b4252"))))
  )

(use-package major-mode-hydra
  :bind ("M-SPC" . major-mode-hydra))

(use-package pretty-hydra
  :demand t
  :preface
  (defun config-hydras--add-quit-bindings (result)
    (append '(("q" nil :exit t)
              ("<escape>" nil :exit t))
            result))
  :config
  (advice-add #'pretty-hydra--get-heads :filter-return #'config-hydras--add-quit-bindings))

(provide 'init-hydra)
