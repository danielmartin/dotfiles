;;; defuns-config.el --- Some useful functions I've written.

;;; Commentary:

 ;;; Code:
(defun dm/recenter-defun ()
  "Recenter the buffer to put the current defun at the top.
 This is somewhat similar to built-in `reposition-window'."
  (interactive)
  (push-mark)
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
  "Toggle between moving the point to the start of the first
 non-whitespace character and the start of the line."
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
    (if (and (stringp string-at-point)
             (string-match "\\`[0-9]*[1-9][0-9]*\\'" string-at-point))
        (goto-char (string-to-number string-at-point))
      (let ((char (read-from-minibuffer "Go to char: ")))
        (goto-char (string-to-number char))))))

(global-set-key (kbd "M-g c") 'dm/dwim-goto-char)

(defvar ediff-do-hexl-diff t
  "Variable used to store trigger for doing diff in hexl-mode")

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

(defun dm/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied buffer file name '%s' to the clipboard." filename))
      (user-error "Buffer does not visit a file, so no filename to copy"))))

(global-set-key (kbd "C-c w") #'dm/copy-file-to-clipboard)

(defun dm/git-history-of-defun ()
  "Ask source control about the history of the function under point."
  (interactive)
  (let ((start (save-excursion
                 (beginning-of-defun)
                 (line-number-at-pos (point))))
        (end (save-excursion
               (end-of-defun)
               (line-number-at-pos (point)))))
    (magit-log-buffer-file nil start end)))

(global-set-key (kbd "C-c v") #'dm/git-history-of-defun)

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
  "Sort Dired listing in different ways (by date, size, or name)."
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
  "Print the time of a `compilation-mode' buffer that finished successfully.
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
        (user-error "Cannot calculate compile time.  Did the compilation finish successfully?"))
      (message
       (format "%s took %s"
               mode-name
               (dm/calculate-compile-time end start))))))

(require 'compile)
(define-key compilation-mode-map (kbd "c") #'dm/print-compile-time)

 ;;; http://blogs.fluidinfo.com/terry/2011/11/10/emacs-buffer-mode-histogram/
(defun dm/buffer-mode-histogram ()
  "Display a histogram of Emacs buffer modes."
  (interactive)
  (let* ((totals ())
         (buffers (buffer-list()))
         (total-buffers (length buffers))
         (ht (make-hash-table :test 'equal)))
    (save-excursion
      (dolist (buffer buffers)
        (set-buffer buffer)
        (let
            ((mode-name (symbol-name major-mode)))
          (puthash mode-name (1+ (gethash mode-name ht 0)) ht))))
    (maphash (lambda (key value)
               (setq totals (cons (list key value) totals)))
             ht)
    (setq totals (sort totals (lambda (x y) (> (cadr x) (cadr y)))))
    (with-output-to-temp-buffer "Buffer mode histogram"
      (princ (format "%d buffers open, in %d distinct modes\n\n"
                     total-buffers (length totals)))
      (dolist (item totals)
        (let
            ((key (car item))
             (count (cadr item)))
          (if (equal (substring key -5) "-mode")
              (setq key (substring key 0 -5)))
          (princ (format "%2d %20s %s\n" count key
                         (make-string count ?+))))))))

(defun dm/stringified-mode-line ()
  "Display the mode line in a help buffer so you can inspect its text properties."
  (interactive)
  (with-help-window "*Stringified Mode Line*"
    (with-current-buffer "*Stringified Mode Line*"
      (font-lock-mode -1)
      (insert (format-mode-line mode-line-format)))))

(defun dm/prepend-pspdfkit-assets-folder (file)
  "Prepend the PSPDFKit assets folder to FILE, assuming it's a relative path."
  (concat "~/Projects/PSPDFKit/assets/" file))

(defun dm/open-pdf-at-point ()
  "Open a PDF at point in Emacs, Adobe Acrobat, or PSPDFInspector."
  (interactive)
  ;; Note that some PDF names may have a space in their name. Extend
  ;; `thing-at-point-file-name-chars' so that `thing-at-point'
  ;; correctly extracts those file names.
  (let* ((thing-at-point-file-name-chars (concat thing-at-point-file-name-chars " "))
         (file (thing-at-point 'filename t))
         (extension (file-name-extension file)))
    (unless extension
      (error "There is not a file at point"))
    (unless (string= "pdf" (downcase extension))
      (error "There is not a PDF file at point"))
    (cond ((file-name-absolute-p file) (dm/open-pdf-at-point--internal file))
          (t (dm/open-pdf-at-point--internal (dm/prepend-pspdfkit-assets-folder file))))))

(defun dm/open-in-emacs (file)
  "Open FILE in another window."
  (find-file-other-window file))

(defun dm/open-in-adobe-acrobat (file)
  "Open FILE in Adobe Acrobat."
  ;; This function assumes that it's running on macOS.
  (call-process "open" nil nil nil "-b" "com.adobe.acrobat.Pro" (expand-file-name file)))

(defun dm/open-in-pspdfinspector (file)
  "Open FILE in PSPDFInspector."
  ;; This function assumes that it's running on macOS.
  (call-process "open" nil nil nil "-a" "PSPDFInspector" (expand-file-name file)))

(defun dm/open-pdf-at-point--internal (file)
  "Prompt which program should open the PDF FILE."
  (let ((programs '(("Emacs" . dm/open-in-emacs)
                    ("Adobe Acrobat" . dm/open-in-adobe-acrobat)
                    ("PSPDFInspector" . dm/open-in-pspdfinspector))))
    (unless (file-readable-p file)
      (error "The file %s does not exist" file))
    (funcall
     (cdr (assoc (completing-read (format "Select a program to open '%s': " file)
                                  programs)
                 programs))
     file)))

(global-set-key (kbd "C-c o") #'dm/open-pdf-at-point)

(defun dm/toggle-invisible-characters ()
  "Toggle the display of characters that are not visible but are
 important, like trailing whitespace."
  (interactive)
  (if (bound-and-true-p whitespace-mode)
      (whitespace-mode -1)
    (whitespace-mode)))

(global-set-key (kbd "<f6>") #'dm/toggle-invisible-characters)

(defun dm/describe-face (pos)
  "Print the name of face or faces at POS in the echo area.

 This is a lightweight alternative to `what-cursor-position' and
 its prefixed variant when you are only interested in debugging
 faces."
  (interactive "d")
  (let ((face (or (get-char-property pos 'face)
                  (get-char-property pos 'read-cf-name))))
    (message "%s" (or face
                      "No face at point"))))

(defun dm/ocr-prepare-image--default (file page)
  "Render an image from FILE at PAGE using a default procedure that works well in most cases."
  ;; convert -density 300 Guide.pdf[6] ~/Downloads/Image.tif
  (let ((output (make-temp-file (file-name-base file) nil ".tif")))
    (call-process "convert"
                  nil nil nil
                  "-density" "300" (format "%s[%s]" file page) "-background" "white" "-alpha" "remove" output)
    output))

(defvar dm/ocr-prepare-image #'dm/ocr-prepare-image--default
  "Function that prepares an image to perform OCR on it.")

(defun dm/ocr-perform-ocr--default (file image)
  "Perform OCR on a given IMAGE using a default procedure that works well in most cases."
  ;; tesseract -l eng Guide.tif Guide
  (let ((output (make-temp-file (file-name-base file))))
    (call-process "tesseract"
                  nil nil nil
                  "-l" "eng" image output)
    output))

(defvar dm/ocr-perform-ocr #'dm/ocr-perform-ocr--default
  "Function that performs OCR on a given image.")

(defun dm/ocr-current-page ()
  "Perform OCR on the current PDF page using Tesseract."
  (interactive)
  (unless buffer-file-name
    (error "The buffer is not visiting a file"))
  (unless (eq major-mode 'pdf-view-mode)
    (error "The buffer is not in `pdf-view-mode'"))
  (let* ((page (number-to-string (1- (pdf-view-current-page))))
         ;; PDF -> TIFF
         (image (funcall dm/ocr-prepare-image buffer-file-name page))
         ;; TIFF -> TXT (OCR)
         (ocr (funcall dm/ocr-perform-ocr buffer-file-name image)))
    ;; Note that Tesseract automatically adds a .txt extension to the
    ;; output file.
    (find-file-other-window (concat ocr ".txt"))))

(defun dm/after-compilation-finished (buf status)
  "Function that will be invoked when a compilation has finished."
  (let ((template (cdr (assoc "Debug PSPDFKit Core Tests" dap-debug-template-configurations))))
    (plist-put template :args core-tests-argument-list)
    (print status)
    (remove-hook 'compilation-finish-functions #'dm/after-compilation-finished)
    ;; Launch the target under a debugger.
    (dap-debug template)))

(defun dm/debug-pspdfkit-core-tests (args)
  "Debug PSPDFKit Core tests, passing ARGS, optionally."
  (interactive "sTest arguments: ")
  (setq core-tests-argument-list (list args))
  ;; Compile the target to make sure it's up to date.
  (add-hook 'compilation-finish-functions #'dm/after-compilation-finished)
  (compile projectile-project-compilation-cmd))

(defun dm/rename-file (new)
  "Rename file OLD to NEW, renaming the buffer as well.
If the file is under version control, act like `vc-rename-file'."
  (interactive "FRename to: ")
  (let ((oldbuf (current-buffer))
        (oldbufname (buffer-name))
        (old (buffer-file-name))
        vc)
    (if (and oldbuf (buffer-modified-p oldbuf))
        (error "Please save files before moving them"))
    (if (get-file-buffer new)
        (error "Already editing new file name"))
    (if (file-exists-p new)
        (error "New file already exists"))
    (let ((state (vc-state old)))
      (cond ((memq state '(up-to-date edited))
             (setq vc t)
             (vc-call rename-file old new)
             (vc-file-clearprops old))
            (state
             (error "Please %s files before moving them"
                    (if (stringp state) "check in" "update")))))
    (if (file-exists-p old) (rename-file old new))
    (with-current-buffer oldbuf
      (let ((buffer-read-only buffer-read-only))
        (set-visited-file-name new))
      (when vc
        (vc-backend new)
        (vc-mode-line new))
      (set-buffer-modified-p nil))
    (message "Renamed %s to %s%s"
             oldbufname new (if vc " [with VC]" ""))))

(global-set-key (kbd "C-c r") 'dm/rename-file)

(defun dm/find-non-ascii-chars-in-buffer ()
  "Use Occur to find non-ASCII characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(defun dm/lookup-password (host user port)
  "Search for HOST USER PORT credentials using `auth-source'."
  (require 'auth-source)
  (require 'auth-source-pass)
  (let ((auth (auth-source-search :host host :user user :port port)))
    (if auth
        (let ((secretf (plist-get (car auth) :secret)))
          (if secretf
              (funcall secretf)
            (error "Auth entry for %s@%s:%s has no secret!"
                   user host port)))
      (error "No auth entry found for %s@%s:%s" user host port))))

(provide 'defuns-config)

;;; defuns-config.el ends here
