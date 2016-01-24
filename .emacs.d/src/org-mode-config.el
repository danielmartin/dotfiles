(setq org-log-done 'time)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(global-font-lock-mode 1)		     ; for all buffers
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; org-mode buffers only

(provide 'org-mode-config)
