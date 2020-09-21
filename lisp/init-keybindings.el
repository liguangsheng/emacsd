(when (fboundp 'which-key-add-key-based-replacements)
  (which-key-add-key-based-replacements
    "SPC b" "buffer"
    "SPC c" "comment"
    "SPC e" "expand"
    "SPC f" "file"
    "SPC h" "helm"
    "SPC m" "mode"
    "SPC p" "projectile"
    "SPC q" "quit"
    "SPC t" "treemacs"
    "SPC w" "window"
    ))

(when (fboundp 'bind-keys)
  (bind-keys
   ("≈"       . helm-M-x)
   ("C-j"     . ace-window)
   ("C-x C-f" . helm-find-files)
   ("C-M-l"   . indent-whole-buffer)
   ("s-/"     . comment-line))

  (bind-keys
   :map evil-normal-state-map
   ("J"  . evil-scroll-page-down)
   ("K"  . evil-scroll-page-up)
   ("u"  . undo-tree-undo)
   ("U"  . undo-tree-redo)
   ("gj" . evil-join))

  (bind-keys
   :map evil-insert-state-map
   ("C-e" . move-end-of-line)
   ("C-a" . move-beginning-of-line)
   ))

(when (fboundp 'evil-leader/set-key)
  (evil-leader/set-key
    ;; file
    "fw"	'save-buffer
    "fe"	'open-init-el
    "fs"	'save-buffer

    ;; buffer
    "bd"	'kill-this-buffer
    "bD"	'kill-all-buffers-i
    "bs"	'switch-to-scratch
    "bi"	'open-inbox
    "bm"	'switch-to-modified-buffer

    ;; window
    "wd"	'delete-window
    "wn"	'other-window
    "wo"	'delete-other-windows
    "w-"	'split-window-below
    "w|"	'split-window-right
    "ww"	'ace-window
    "w SPC"	'ace-window

    ;; comment
    "cl"	'comment-line
    "cc"	'comment-dwim

    ;; quit
    "qq"	'save-buffers-kill-emacs
    ))

(provide 'init-keybindings)
