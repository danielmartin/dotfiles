(setq org-directory "~/Org-Mode")
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
(define-key global-map "\C-cL" 'org-insert-link-global)
(define-key global-map "\C-cr" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

;; Org-Mode Capture
(setq org-default-notes-file "~/Org-Mode/organizer.org")

;; Org-Mode Agenda
(setq org-agenda-files '("~/Org-Mode/"))

;; Org-Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (restclient . t)
   (swift . t)))

(global-font-lock-mode 1)		     ; for all buffers
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; org-mode buffers only

(provide 'org-mode-config)
