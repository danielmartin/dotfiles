;; Nice option to find recent files
(global-set-key (kbd "C-c f") 'recentf-ido-find-file)
;; Recenter-defun is C-c d
(global-set-key "\C-cd" 'recenter-defun)
;; Other-window is M-o
(global-set-key "\M-o" 'other-window)
;; Redefine C-x C-b to buffer-menu
(global-set-key "\C-x\C-b" 'buffer-menu)
;; Indent regions more easily
(global-set-key "\C-ci" 'indent-region)

(provide 'keybindings)
