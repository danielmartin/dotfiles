;;; org-mode-config.el --- Org-Mode configuration file.

;;; Commentary:

;;; Code:

;;
;; General Org-Mode configuration.
;;
(use-package org-mode
  :bind (("C-c l" . org-store-link)
         ("C-c L" . org-insert-link-global)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :init
  (setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")
  (setq org-fontify-done-headline t)
  (setq org-src-fontify-natively t)
  (setq org-log-done 'time)
  ;;
  ;; Org-Mode agenda configuration.
  ;;
  (setq org-agenda-files
        '("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/todo.org"
          "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/OSS.org"
          "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/PSPDFKit.org"
          "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/self-employment.org"))
  (setq org-refile-targets '((org-agenda-files :todo . "PROJECT")))

  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  ;;
  ;; Org-Mode capture configuration.
  ;;
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
  (defun dm/org-get-safari-link ()
    (let ((subject (substring (do-applescript
                               (string-to-multibyte "tell application \"Safari\"
        name of document of front window
end tell")) 1 -1))
          (url (substring (do-applescript
                           (string-to-multibyte "tell application \"Safari\"
        URL of document of front window
end tell")) 1 -1)))
      (org-make-link-string url subject)))

  (defun dm/org-insert-url-link ()
    (interactive)
    (insert (dm/org-get-safari-link)))

(use-package emacs
  :config
  (defun dm/load-org-babel-languages ()
    "Load `org-babel` languages on demand.
The main reason for this function is to avoid loading `org.el` at
startup time, which is expensive."
    (interactive)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)
       (dot . t)
       (C . t)))))

(provide 'org-mode-config)
;;; org-mode-config.el ends here
