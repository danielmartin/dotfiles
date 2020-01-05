;;; appearance.el --- Appearance and theme configuration.

;;; Commentary:

;;; Code:

;; Ditch the splash screen.
(setq inhibit-splash-screen t)

;; On graphical systems, do not show a toolbar and show the buffer
;; path in the frame's title bar.
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tool-bar-mode -1))

;; Ditch scrollbars.
(scroll-bar-mode -1)

(provide 'appearance)
;;; appearance.el ends here
