;;; keybindings.el --- General customized keybindings.

;;; Commentary:

;;; Code:

;; Recenter-defun is C-c d
(global-set-key "\C-cd" #'dm/recenter-defun)
;; Other-window is M-o
(global-set-key "\M-o" #'other-window)
;; Redefine C-x C-b to buffer-menu
(global-set-key "\C-x\C-b" #'buffer-menu)
;; Indent regions more easily
(global-set-key "\C-ci" #'indent-region)
;; Open newlines above current position
(global-set-key (kbd "C-S-o") #'dm/open-line-above)
;; Resize windows
(global-set-key (kbd "C-x <up>") #'shrink-window)
(global-set-key (kbd "C-x <down>") #'enlarge-window)
(global-set-key (kbd "C-x <left>") #'shrink-window-horizontally)
(global-set-key (kbd "C-x <right>") #'enlarge-window-horizontally)
;; Scroll windows
(global-set-key (kbd "C-z") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-z") (lambda () (interactive) (scroll-down 1)))

(provide 'keybindings)
;;; keybindings.el ends here
