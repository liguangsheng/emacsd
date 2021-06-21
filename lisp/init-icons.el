(defvar prefer-icons nil)

(use-package all-the-icons
  ;; :commands (all-the-icons-alltheicon
  ;; 	     all-the-icons-faicon
  ;; 	     all-the-icons-fileicon
  ;; 	     all-the-icons-octiocn
  ;; 	     all-the-icons-material)
  ;; :preface
  ;; (defun with-alltheicon (icon str &optional height v-adjust)
  ;;   "Displays an icon from all-the-icon."
  ;;   (if prefer-icons (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)
  ;;     str))

  ;; (defun with-faicon (icon str &optional height v-adjust)
  ;;   "Displays an icon from Font Awesome icon."
  ;;   (if prefer-icons (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)
  ;;     str))

  ;; (defun with-fileicon (icon str &optional height v-adjust)
  ;;   "Displays an icon from the Atom File Icons package."
  ;;   (if prefer-icons (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)
  ;;     str))

  ;; (defun with-octicon (icon str &optional height v-adjust)
  ;;   "Displays an icon from the GitHub Octicons."
  ;;   (if prefer-icons (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)
  ;;     str))

  ;; (defun with-material (icon str &optional height v-adjust)
  ;;   "Displays an icon from all-the-icon."
  ;;   (if prefer-icons (s-concat (all-the-icons-material icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)
  ;;     str))

  ;; (defun insert-all-icons ()
  ;;   (interactive)
  ;;   (dolist (item (append all-the-icons-icon-alist
  ;; 			  all-the-icons-dir-icon-alist
  ;; 			  all-the-icons-weather-icon-alist
  ;; 			  all-the-icons-mode-icon-alist)
  ;; 		  (insert (format "%s %s\n" (nth 2 item) (funcall (nth 1 item) (nth 2 item) :v-adjust 0 :height 1))))))

  ;; (defun random-choice-icon ()
  ;;   (let ((item (random-choice all-the-icons-icon-alist)))
  ;;     (funcall (nth 1 item) (nth 2 item) :v-adjust 0 :height 1)))

  ;; (defun with-random-icon (s)
  ;;   (s-concat (random-choice-icon) " " s))
  )

;; (insert (random-choice-icon))

(provide 'init-icons)
