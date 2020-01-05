;;; org-mode-config.el --- Org-Mode configuration file.

;;; Commentary:

;;; Code:
(setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")
(setq org-fontify-done-headline t)
(setq org-src-fontify-natively t)
(setq org-log-done 'time)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cL" 'org-insert-link-global)
(define-key global-map "\C-cr" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

;; Org-Mode Capture Templates.
(setq org-capture-templates
    '(("a" "Add Task" entry
      (file+headline "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/todo.org" "Inbox")
      "* TODO %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("n" "Note" entry
      (file "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/notes.org")
      "* NOTE %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("c" "Calendar" entry
      (file+headline "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/todo.org" "Inbox")
      "* APPT %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("t" "Add Task" entry
      (file+headline "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/todo.org" "Inbox")
      "* TODO %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("p" "Protocol" entry
      (file+headline "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/todo.org" "Inbox")
      "* NOTE %?
#+BEGIN_QUOTE
%i
#+END_QUOTE
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:URL:      %c
:END:")
     ("L" "Protocol Link" entry
      (file+headline "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/todo.org" "Inbox")
      "* NOTE %?
[[%:link][%:description]]
#+BEGIN_QUOTE
%i
#+END_QUOTE
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:URL:      %c
:END:")))

(define-key global-map "\C-cc" 'org-capture)

;; Org-Mode Agenda
(setq org-agenda-files
      '("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/todo.org"
        "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/OSS.org"
        "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/PSPDFKit.org"
        "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/self-employment.org"))

;; Org-Mode Refile
(setq org-refile-targets '((org-agenda-files :todo . "PROJECT")))

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
