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

  (defun dm/org-todo-age-time (&optional pos)
    (let ((stamp (org-entry-get (or pos (point)) "CREATED" t)))
      (when stamp
        (time-subtract (current-time)
                       (org-time-string-to-time
                        (org-entry-get (or pos (point)) "CREATED" t))))))

  (defun dm/org-todo-age (&optional pos)
    (let ((days (time-to-number-of-days (dm/org-todo-age-time pos))))
      (cond
       ((< days 1)   "today")
       ((< days 7)   (format "%dd" days))
       ((< days 30)  (format "%.1fw" (/ days 7.0)))
       ((< days 358) (format "%.1fM" (/ days 30.0)))
       (t            (format "%.1fY" (/ days 365.0))))))

  (setq org-agenda-prefix-format
        '((agenda . "  %-13c%?-12t% s")
          (timeline . "  % s")
          (todo . "  %-13c%5(dm/org-todo-age) ")
          (tags . "  %-13c")))
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
  ;;
  ;; Org-Agenda custom commands.
  ;;
  (defun dm/org-current-is-todo ()
    (member (org-get-todo-state) '("TODO" "EPIC" "STORY" "STARTED")))

  (defun dm/org-agenda-should-skip-p ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (dm/org-current-is-todo)
        (setq should-skip-entry t))
      (when (or (org-get-scheduled-time (point))
                (org-get-deadline-time (point)))
        (setq should-skip-entry t))
      (when (/= (point)
                (save-excursion
                  (org-goto-first-child)
                  (point)))
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (and (dm/org-current-is-todo)
                     (not (org-get-scheduled-time (point)))
                     (not (org-get-deadline-time (point))))
            (setq should-skip-entry t))))
      should-skip-entry))

  (defun dm/org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (when (dm/org-agenda-should-skip-p)
      (or (outline-next-heading)
          (goto-char (point-max)))))

  (setq org-agenda-custom-commands
        '(("n" "Project Next Actions" alltodo ""
           ((org-agenda-overriding-header "Project Next Actions")
            (org-agenda-skip-function
             #'dm/org-agenda-skip-all-siblings-but-first)))))

  ;; Custom functions
  (defun dm/org-get-safari-link ()
    (let ((subject (substring (do-applescript
                               (string-to-multibyte "tell application \"Safari\"
        name of document of front window
end tell")) 1 -1))
          (url (substring (do-applescript
                           (string-to-multibyte "tell application \"Safari\"
        URL of document of front window
end tell")) 1 -1)))
      (org-link-make-string url subject)))

  (defun dm/org-insert-url-link ()
    (interactive)
    (insert (dm/org-get-safari-link)))

  (defun dm/org-wrap-region-for-init ()
    "Wrap a region of initialization code as an Org block."
    (interactive)
    (save-excursion
      (goto-char (region-end))
      (insert "\n#+END_SRC\n")
      (goto-char (region-beginning))
      (insert "#+BEGIN_SRC emacs-lisp :tangle yes :comments org\n"))))

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
