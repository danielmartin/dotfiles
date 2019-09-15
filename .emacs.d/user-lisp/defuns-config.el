;;; defuns-config.el --- Some useful functions I've written.

;;; Commentary:

;;; Code:
(defun dm/recenter-defun()
  "Recenter the buffer to put the current defun at the top."
  (interactive)
  (beginning-of-defun)
  (let ((this-scroll-margin
	 (min (max 0 scroll-margin)
	      (truncate (/ (window-body-height) 4.0)))))
    (recenter this-scroll-margin)))

(defun dm/open-line-above (N)
    "Insert a new line above the current point position.
With arg N, insert N newlines."
    (interactive "p")
    (save-excursion
      (beginning-of-line)
      (newline N)))

(defun dm/beautify-json ()
  "Pretty print a region containing a JSON structure."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

(defun dm/beginning-of-line-dwim ()
  "Toggle between moving the point to the start of the first non-whitespace character and the start of the line."
  (interactive)
  (let ((start-position (point)))
    ;; Move to the first non-whitespace character.
    (back-to-indentation)
    ;; If we haven't moved, go to the start of the line.
    (when (= (point) start-position)
      (move-beginning-of-line nil))))

(global-set-key (kbd "C-a") 'dm/beginning-of-line-dwim)

(defun dm/dwim-goto-char ()
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

(global-set-key (kbd "M-g c") 'dm/dwim-goto-char)

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

(defun dm/copy-file-to-clipboard ()
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

(global-set-key (kbd "C-c w") 'copy-file-to-clipboard)

(defun dm/git-history-of-defun ()
  "Ask source control about the history of the function under
  point."
  (interactive)
  (mark-defun)
  (magit-log-buffer-file nil (line-number-at-pos (region-beginning)) (line-number-at-pos (region-end))))

(global-set-key (kbd "C-c v") 'dm/git-history-of-defun)

(defun dm/compile-finish (buf status)
  (with-current-buffer buf
    (if (string-match "^finished\\b" status)
        (ns-do-applescript (format
                            "display notification \"Compilation successful\" with title \"%s\""
                            (projectile-project-name)))
      (ns-do-applescript (format
                          "display notification \"Compilation failed\" with title \"%s\""
                          (projectile-project-name))))))

(add-hook 'compilation-finish-functions #'dm/compile-finish)

(defun dm/dired-sort ()
  "Sort Dired listing in different ways (by date, size, or
  name)."
  (interactive)
  (setq sort-by (ido-completing-read "Sort by: " '("date" "size" "name")))
  (cond
   ((equal sort-by "name") (setq ls-flags "-Al "))
   ((equal sort-by "date") (setq ls-flags "-Al -t"))
   ((equal sort-by "size") (setq ls-flags "-Al -S")))
  (dired-sort-other ls-flags))

(require 'dired)
(define-key dired-mode-map (kbd "s") #'dm/dired-sort)

(defun dm/copy-as-rtf ()
  "Export region to RTF and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf))))

(defun dm/calculate-compile-time (start-time end-time)
  "Compute the compile/grep time between START-TIME and END-TIME.
Time is formatted in hours, minutes, and seconds."
  (format-seconds
   "%H %M, %z%S"
   (time-to-seconds
    (time-subtract
     (apply #'encode-time (parse-time-string (format-time-string
                                              (concat
                                               start-time
                                               " %Y"))))
     (apply #'encode-time (parse-time-string (format-time-string
                                              (concat
                                               end-time
                                               " %Y"))))))))

(defun dm/print-compile-time ()
  "Print the compile/grep time of a `compilation-mode' buffer that finished successfully.
Time is formatted in hours, minutes, and seconds."
  (interactive)
  (when (derived-mode-p 'compilation-mode)
    (let ((start (save-excursion
                   (goto-char (point-min))
                   (and
                    (re-search-forward
                     (format
                      "^%s started at \\(.*\\)"
                      mode-name) nil t)
                    (match-string 1))))
          (end (save-excursion
                 (goto-char (point-min))
                 (and
                  (re-search-forward
                   (format
                    "^%s finished at \\(.*\\)"
                    mode-name) nil t)
                  (match-string 1)))))
      (unless (and start end)
        (user-error "Cannot calculate compile time. Did the compilation finished successfully?"))
      (message
       (format "%s took %s"
               mode-name
               (dm/calculate-compile-time end start))))))

(require 'compile)
(define-key compilation-mode-map (kbd "c") #'dm/print-compile-time)

(provide 'defuns-config)
;;; defuns-config.el ends here
