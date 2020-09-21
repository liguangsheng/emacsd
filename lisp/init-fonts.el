(defvar en-fonts '("Menlo" 12 "Courier New" 12))
(defvar cn-fonts '("PingFang SC" 12 "宋体" 12 "微软雅黑" 12))

(defun font/exists-p (font-name)
  "Check if font exists."
  (if (null (x-list-fonts font-name)) nil t))

(defun font/use-en (font-name font-size)
  "Set font for english."
  (set-face-attribute 'default nil
		      :font (format "%s:pixelsize=%d" font-name font-size)
		      :weight 'normal))

(defun font/use-cn (font-name font-size)
  "Set font for chinese."
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
		      (font-spec :family font-name :size font-size))))

(defun font/use-list (font-list font-func)
  "Search font in font-list, use it if exists."
  (unless (null font-list)
    (let ((font-name (car font-list))
	  (font-size (cadr font-list)))
      (if (font/exists-p font-name)
	  (funcall font-func font-name font-size)
	(font/use-list (cddr font-list) font-func)))))

(defun font/use-en-list (font-list)
  (font/use-list font-list 'font/use-en))

(defun font/use-cn-list (font-list)
  (font/use-list font-list 'font/use-cn))

(defun init-font ()
  (when (display-graphic-p)
    (font/use-en-list en-fonts)
    (font/use-cn-list cn-fonts)))

(init-font)

(provide 'init-fonts)
