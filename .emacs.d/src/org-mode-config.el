(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
                             :weight normal
                             :strike-through t))))
 '(Org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon" :strike-through t)))))

(setq org-log-done 'time)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(global-font-lock-mode 1)		     ; for all buffers
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; org-mode buffers only

(provide 'org-mode-config)
