(require 'subr-x)
(require 'cl-lib)

(defconst user-emacs-lisp-directory
  (expand-file-name "lisp/" user-emacs-directory)
  "Path to .emacs.d/lisp directory where init files exists.")

(defconst user-emacs-site-lisp-directory
  (expand-file-name "site-lisp/" user-emacs-directory)
  "Path to .emacs.d/site-lisp directory.")

(defconst user-emacs-cache-directory
  (expand-file-name ".cache/" user-emacs-directory)
  "Path to .emacs.d/.cache directory.")

;; Add dir to load-path
(add-to-list 'load-path user-emacs-lisp-directory)
(add-to-list 'load-path user-emacs-site-lisp-directory)

;; Recursive add site-lisp to load-path
(let ((default-directory user-emacs-site-lisp-directory))
  (normal-top-level-add-subdirs-to-load-path))

(defun make-directory-unless-exists (dir)
  "Make directory unless DIR is already exists"
  (unless (file-executable-p dir) (make-directory dir)))

(make-directory-unless-exists user-emacs-cache-directory)

(defun u-site-lisp (filename)
  (expand-file-name filename user-emacs-site-lisp-directory))

;; OS Environment see http://ergoemacs.org/emacs_manual/elisp/System-Environment.html
(setq *mac* (eq system-type 'darwin)
      *win64* (eq system-type 'windows-nt)
      *cygwin* (eq system-type 'cygwin)
      *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux))
      *emacs26* (>= emacs-major-version 26)
      *emacs27* (>= emacs-major-version 27))

(defun windows-total-physical-memory ()
  (/ (float (string-to-number 
	     (nth 1 (split-string (shell-command-to-string "wmic computersystem get TotalPhysicalMemory") "\n"))))
     1024 1024 1024))

(setq total-physical-memory
      (cond (*win64* (windows-total-physical-memory))))

(setq *no-money-and=cry* (< total-physical-memory 32)
      *i-am-rich* (> total-physical-memory 32))

(provide 'init-core)

;;; init-core.el ends here
