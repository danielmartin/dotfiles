;; define a function to use recentf from ido
(defun recentf-ido-find-file ()
  "Open a recent file using IDO mode"
  (interactive)
  (let ((file (ido-completing-read "Recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; recenter buffer so that current defun is at the top
(defun recenter-defun()
  "Recenters the buffer to put the current defun at the top."
  (interactive)
  (beginning-of-defun)
  (let ((this-scroll-margin
	 (min (max 0 scroll-margin)
	      (truncate (/ (window-body-height) 4.0)))))
    (recenter this-scroll-margin)))

(defun expand-and-indent-block ()
  "Expand a statement block so that the point is inside the block in a
new, indented line, and the closing brace of the block is also indented"
  (interactive "*")
  (delete-horizontal-space t)
  (newline)
  (newline)
  (indent-according-to-mode)
  (previous-line)
  (indext-for-tab-command))

(defun open-line-above (N)
    "Insert a new line above the current point position.
With arg N, insert N newlines."
    (interactive "p")
    (save-excursion
      (beginning-of-line)
      (newline N)))

(defun beautify-json ()
  "Pretty prints a region containing a JSON structure"
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

(provide 'defuns-config)
