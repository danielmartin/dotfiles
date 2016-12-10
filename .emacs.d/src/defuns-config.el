;;; defuns-config.el --- Some useful, custom functions

;;; Commentary:

;;; Code:
(defun recenter-defun()
  "Recenter the buffer to put the current defun at the top."
  (interactive)
  (beginning-of-defun)
  (let ((this-scroll-margin
	 (min (max 0 scroll-margin)
	      (truncate (/ (window-body-height) 4.0)))))
    (recenter this-scroll-margin)))

(defun open-line-above (N)
    "Insert a new line above the current point position.
With arg N, insert N newlines."
    (interactive "p")
    (save-excursion
      (beginning-of-line)
      (newline N)))

(defun beautify-json ()
  "Pretty print a region containing a JSON structure."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

(provide 'defuns-config)
;;; defuns-config.el ends here
