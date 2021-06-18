;; Font Example:
;; 春眠不觉晓，处处闻啼鸟
;; abcdefghijklmnopqrstuvwxyz
;; ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; 0123456789

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

;; (use-font
;;  :font-name "Go Mono for Powerline"
;;  :font-size 12
;;  :chinese-font-name "WenQuanYi Micro Hei"
;;  :chinese-font-size 12
;;  )

(setq-default cjk-charsets '(kana han symbol cjk-misc bopomofo)
	      cn-fonts `(,(font-spec :family "WenQuanYi Micro Hei" :height 90)
			 ,(font-spec :family "Microsoft Yahei" :height 90))
	      en-fonts '("Droid Sans Mono:size=14"
			 "Menlo:size=14"
			 "Monoco:size=14"
			 "Consolas:size=14"
			 "Courier New:size=14"
			 "monospace:size=14"))


(defvar prefer-en-font nil)
(when (or (stringp prefer-en-font) (fontp prefer-en-font)) (push prefer-en-font en-fonts))
(defvar prefer-cn-font nil)
(when (or (stringp prefer-cn-font) (fontp prefer-cn-font)) (push prefer-cn-font cn-fonts))

(defun reset-font ()
  (interactive)
  ;; 设置unicode字体
  (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
           when (font-installed-p font)
           return(set-fontset-font t 'unicode font nil 'prepend))

  ;; 设置英文字体
  (cl-loop for font in en-fonts
	   when (font-installed-p font)
	   return (set-frame-font font))

  ;; 设置中文字体
  (cl-loop for font in cn-fonts
	   do (cl-loop for charset in cjk-charsets
		       do (set-fontset-font t charset font)))
  )

(when (display-graphic-p)
  (add-hook 'after-init-hook 'reset-font)
  (add-hook 'minibuffer-setup-hook
	    (lambda ()  (set (make-local-variable 'face-remapping-alist) '((default :height 90))))))

(provide 'init-fonts)

