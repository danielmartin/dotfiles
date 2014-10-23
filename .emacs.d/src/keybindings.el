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

;; Helm
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

;; Redefine M-x to use Helm
(global-set-key (kbd "M-x") 'helm-M-x)
;; We want a more useful kill ring cycling
(global-set-key (kbd "C-y") 'helm-show-kill-ring)
;; File navigation on steroid
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(provide 'keybindings)
