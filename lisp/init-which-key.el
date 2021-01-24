;;; init-which-key.el

;;; Commentary:

;;; Code:

(use-package which-key
  :init
  (setq which-key-popup-type 'side-window
	which-key-side-window-location 'bottom
	which-key-idle-delay 0.4
	which-key-separator " → "
	which-key-prefix-prefix "+"
	which-key-side-window-max-heght 0.25)
  :config
  (which-key-mode 1))

(use-package which-key-posframe
  :disabled t
  :if prefer-posframe
  :init (setq which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-center
	      which-key-posframe-border-width 2
	      which-key-posframe-parameters '((left-fringe . 5) (right-fringe . 5)))
  :config (which-key-posframe-mode))

(provide 'init-which-key)
