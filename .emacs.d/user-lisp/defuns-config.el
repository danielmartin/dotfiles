;;; defuns-config.el --- Some useful functions I've written.

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

(defun beginning-of-line-dwim ()
  "Toggle between moving the point to the start of the first non-whitespace character and the start of the line."
  (interactive)
  (let ((start-position (point)))
    ;; Move to the first non-whitespace character.
    (back-to-indentation)
    ;; If we haven't moved, go to the start of the line.
    (when (= (point) start-position)
      (move-beginning-of-line nil))))

(global-set-key (kbd "C-a") 'beginning-of-line-dwim)

(defun dwim-goto-char ()
  "Goto-char using the offset at point, or fallback to standard
  goto-char if the word at point is not a number. Useful for
  navigating offsets inside raw PDF documents, for example."
  (interactive)
  (let ((string-at-point (thing-at-point 'word)))
    (push-mark)
    (if (and (stringp string-at-point) (string-match "\\`[0-9]*[1-9][0-9]*\\'" string-at-point))
        (goto-char (string-to-number string-at-point))
      (let ((char (read-from-minibuffer "Go to char: ")))
        (goto-char (string-to-number char))))))

(global-set-key (kbd "M-g c") 'dwim-goto-char)

(provide 'defuns-config)
;;; defuns-config.el ends here
