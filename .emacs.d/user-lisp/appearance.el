;;; appearance.el --- Appearance and theme configuration.

;;; Commentary:

;;; Code:

(setq custom-theme-directory (concat user-emacs-directory "user-lisp/themes"))

(defun use-default-theme ()
  (interactive)
  (load-theme 'default-black t))

(defun use-light-theme ()
  (interactive)
  (use-package leuven-theme)
  (load-theme 'leuven t))

(use-default-theme)

;; Ditch the splash screen
(setq inhibit-splash-screen t)

;; Set a nice font
(set-frame-font "Inconsolata-16")

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (blink-cursor-mode -1)
  (tool-bar-mode -1))

;; Ditch scrollbars
(scroll-bar-mode -1)

(provide 'appearance)
;;; appearance.el ends here
