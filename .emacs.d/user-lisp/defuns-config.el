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

(defvar ediff-do-hexl-diff t "Variable used to store trigger for doing diff in hexl-mode")
(defadvice ediff-files-internal (around
ediff-files-internal-for-binary-files activate)
  "Catch the condition when the binary files differ the reason
for catching the error out here (when re-thrown from the inner
advice) is to let the stack continue to unwind before we start
the new diff otherwise some code in the middle of the stack
expects some output that isn't there and triggers an error"
  (let ((file-A (ad-get-arg 0))
        (file-B (ad-get-arg 1))
        ediff-do-hexl-diff)
    (condition-case err
        (progn
          ad-do-it)
      (error
       (if ediff-do-hexl-diff
           (let ((buf-A (find-file-noselect file-A))
                 (buf-B (find-file-noselect file-B)))
             (with-current-buffer buf-A
               (hexl-mode 1))
             (with-current-buffer buf-B
               (hexl-mode 1))
             (ediff-buffers buf-A buf-B))
         (error (error-message-string err)))))))

(defadvice ediff-setup-diff-regions (around ediff-setup-diff-regions-for-binary-files activate)
  "When binary files differ, set the variable "
  (condition-case err
      (progn
        ad-do-it)
    (error
     (setq ediff-do-hexl-diff
           (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                (error-message-string err))
                (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
                (y-or-n-p "The binary files differ, look at the differences in hexl-mode? ")))
     (error (error-message-string err)))))

(defun copy-file-to-keyboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied buffer file name '%s' to the clipboard." filename))
      (user-error "Buffer does not visit a file, so no filename to copy."))))

(global-set-key (kbd "C-c w") 'copy-file-to-keyboard)

(provide 'defuns-config)
;;; defuns-config.el ends here
