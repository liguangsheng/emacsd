(defvar default-font-size 12)
(defvar prefer-fonts '((
			:if (eq system-type 'windows-nt)
			:font-name "Source Code Pro for Powerline"
			:font-size 12
			:chinese-font-name "WenQuanYi Micro Hei"
			:chinese-font-size 14
			)
		       (
			:if (eq system-stype 'windows-nt)
			:font-name "Consolas"
			:font-size 12
			:chinese-font-name "Microsoft YaHei UI"
			:chinese-font-size 14)
		       ))

(defun font-exists-p (font-name)
  "Check if font exists."
  (not (null (x-list-fonts font-name))))

(defun use-font (&rest args)
  "Set font"
  (unless (and (plist-member args :if) (not (plist-get args :if))) 
    (let* ((en-font-name (plist-get args :font-name))
	   (en-font-size (or (plist-get args :font-size) default-font-size))
	   (cn-font-name (plist-get args :chinese-font-name))
	   (cn-font-size (or (plist-get args :chinese-font-size) default-font-size)))
      (and
       (if en-font-name (when (font-exists-p en-font-name)
       			  (set-face-attribute 'default nil
       					      :font (format "%s:pixelsize=%d" en-font-name en-font-size)
       					      :weight 'normal) t) t)

       (if cn-font-name (when (font-exists-p cn-font-name)
			  (dolist (charset '(kana han symbol cjk-misc bopomofo))
			    (set-fontset-font (frame-parameter nil 'font) charset
					      (font-spec :family cn-font-name :size cn-font-size))) t) t)
       ))))

(defun use-font-fallback (font-list)
  "Set fonts fallback"
  (catch 'finish
    (dolist (font font-list)
      (print font)
      (if (apply 'use-font font) (throw 'finish t)))))

;; 春眠不觉晓，处处闻啼鸟
;; abcdefghijklmnopqrstuvwxyz
;; ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; (use-font
;;  :font-name "Go Mono for Powerline"
;;  :font-size 12
;;  :chinese-font-name "WenQuanYi Micro Hei"
;;  :chinese-font-size 12
;;  )

(add-hook 'after-init-hook (lambda () (use-font-fallback prefer-fonts)))

(provide 'init-fonts)

