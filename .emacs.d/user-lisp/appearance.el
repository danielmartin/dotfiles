;;; appearance.el --- Appearance and theme configuration.

;;; Commentary:

;;; Code:

(setq custom-theme-directory (concat user-emacs-directory "user-lisp/themes"))

(defun use-default-theme ()
  (interactive)
  (use-package color-theme-sanityinc-tomorrow
    :ensure t
    :config
    (setf custom-safe-themes t)
    (color-theme-sanityinc-tomorrow-night)
    (setq moody-slant-function #'moody-slant-apple-rgb)
    (let ((line (face-attribute 'mode-line :underline)))
      (set-face-attribute 'mode-line          nil :overline   line)
      (set-face-attribute 'mode-line-inactive nil :overline   line)
      (set-face-attribute 'mode-line-inactive nil :underline  line)
      (set-face-attribute 'mode-line          nil :box        nil)
      (set-face-attribute 'mode-line-inactive nil :box        nil)
      (set-face-attribute 'mode-line-inactive nil :background "#161424"))
    (custom-set-faces
     '(lsp-face-highlight-textual ((t :background "#464752")))
     '(cursor ((t :background "#eebb28"))))))

(defun use-light-theme ()
  (interactive)
  (use-package leuven-theme
    :ensure t
    :defer t)
  (load-theme 'leuven t))

(defun use-solarized-theme ()
  (interactive)
  (use-package solarized-theme
    :config
    (load-theme 'solarized-light t)
    (let ((line (face-attribute 'mode-line :underline)))
      (set-face-attribute 'mode-line          nil :overline   line)
      (set-face-attribute 'mode-line-inactive nil :overline   line)
      (set-face-attribute 'mode-line-inactive nil :underline  line)
      (set-face-attribute 'mode-line          nil :box        nil)
      (set-face-attribute 'mode-line-inactive nil :box        nil)
      (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9"))))

(defun use-doom-theme ()
  (interactive)
  (use-package doom-themes
    :ensure t
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
    ;; may have their own settings.
    (load-theme 'doom-one t)
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme (all-the-icons must be installed!)
    (doom-themes-neotree-config)
    ;; or for treemacs users
    (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)))

(use-default-theme)
;; (use-solarized-theme)
;; (use-doom-theme)

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
