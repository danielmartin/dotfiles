;;; org-mode-config.el --- Org-Mode configuration file.

;;; Commentary:

;;; Code:
(setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")
(setq org-fontify-done-headline t)
(setq org-src-fontify-natively t)
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
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Documents/Org-Mode/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Documents/Org-Mode/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(define-key global-map "\C-cc" 'org-capture)

;; Org-Mode Agenda
(setq org-agenda-files '("~/Documents/Org-Mode/inbox.org"
                         "~/Documents/Org-Mode/gtd.org"
                         "~/Documents/Org-Mode/tickler.org"))

;; Org-Mode Refile
(setq org-refile-targets '(("~/Documents/Org-Mode/gtd.org" :maxlevel . 3)
                           ("~/Documents/Org-Mode/someday.org" :level . 1)
                           ("~/Documents/Org-Mode/tickler.org" :maxlevel . 2)))

;; Org-Mode Todo Keywords
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(defun org-get-safari-link ()
  (let ((subject (substring (do-applescript
                             (string-to-multibyte "tell application \"Safari\"
        name of document of front window
end tell")) 1 -1))
        (url (substring (do-applescript
                         (string-to-multibyte "tell application \"Safari\"
        URL of document of front window
end tell")) 1 -1)))
    (org-make-link-string url subject)))

(defun org-insert-url-link ()
  (interactive)
  (insert (org-get-safari-link)))

;; Org-Babel

;; Swift and Objective C.
(use-package ns-playgrounds
  :load-path "~/Projects/ns-playgrounds"
  :config
  (setq ob-swift-prompt-if-no-toolchain t)
  (setq ob-swift-debug-compiler-path "~/Projects/swift-source/build/Ninja-RelWithDebInfoAssert+swift-DebugAssert/swift-macosx-x86_64/bin/swift"))

;; Rust
(use-package ob-rust
  :ensure t)

 (org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (shell . t)
    (dot . t)
    (C . t)))


(global-font-lock-mode 1)		     ; for all buffers
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; org-mode buffers only

(require 'ox-latex)

(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))

;; Keep an up-to-date table of contents:
(use-package toc-org
  :ensure t
  :hook (org-mode-hook . 'toc-org-mode))

(provide 'org-mode-config)
;;; org-mode-config.el ends here
