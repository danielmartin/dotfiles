;; Custom color scheme, to reduce eye strain
(use-package tangotango-theme
  :ensure t
  :init
  (load-theme 'tangotango t))

;; Ditch the splash screen
(setq inhibit-splash-screen t)

;; Set a nice font
(set-frame-font "Monaco-13")

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (blink-cursor-mode -1)
  (tool-bar-mode -1))

;; Ditch scrollbars
(scroll-bar-mode -1)

(provide 'appearance)
