(require 'cl)

(defun hook-gross-modes (function &optional depth local)
  (add-hook 'prog-mode-hook        function depth local)
  (add-hook 'fundamental-mode-hook function depth local)
  (add-hook 'text-mode-hook        function depth local))

(defun indent-whole-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun move-to-front (list x)
  (cons x (remove x list)))

(defun kill-all-buffers (KILL-STARRED-BUFFER)
  "Kill all buffers."
  (dolist (buffer (buffer-list))
    (let ((bname (string-trim (buffer-name buffer))))
      (if (and (not KILL-STARRED-BUFFER)
	       (string-prefix-p "*" bname)
	       (string-suffix-p "*" bname))
	  nil
	(kill-buffer-if-not-modified buffer)
	))))

(defun kill-all-buffers-i ()
  (interactive)
  (kill-all-buffers nil))

(defun switch-to-modified-buffer ()
  "Switch to modified buffer"
  (interactive)
  (let ((buf-list
	 (seq-filter
	  (lambda (x) (not (or
			    (not (buffer-modified-p x))
			    (s-prefix? "*" (buffer-name x))
			    (s-prefix? " *" (buffer-name x))
			    (s-suffix? "-mode" (buffer-name x)))))
	  (buffer-list))))
    (if buf-list
	(switch-to-buffer (first buf-list))
      (message "No buffer modified."))))

(defun open-init-el ()
  "Open ~/.emacs.d/init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-inbox ()
  "Open ~/INBOX"
  (interactive)
  (find-file "~/INBOX"))

(defun switch-to-scratch ()
  "Swtich to *scratch* buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun random-choice (LIST)
  "Get a random choice from LIST"
  (nth (mod (random) (length LIST)) LIST))

(defun random-theme ()
  "Return a random theme symbol"
  (random-choice (custom-available-themes)))

(defmacro withf (func &rest body)
  (declare (indent 1) (debug t))
  `(when (fboundp ,func) ,@body))

(provide 'init-my-functions)
