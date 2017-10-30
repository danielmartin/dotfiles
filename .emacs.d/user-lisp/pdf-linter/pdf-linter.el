;;; pdf-linter.el --- PDF document linter using PDFBox Preflight app.

;; Author: Daniel Martín
;; Keywords: productivity
;; URL: https://github.com/danielmartin/pdf-linter
;; Emacs: GNU Emacs 24 or later
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ## Install:

;; Put the following expression into your initialization file:

;;     (require 'pdf-linter)
;;     (setq pdf-linter-jar "path/to/pdfbox/preflight.jar")

;; ## Usage:

;; Run the following command to lint the PDF file loaded in the current buffer:

;;     M-x pdf-lint

;;; Code:

(defgroup pdf-linter nil
  "Linting for PDF files."
  :prefix "pdf-linter-"
  :group 'applications)

(defcustom pdf-linter-log-buffer "*pdf-linter*"
  "The buffer in which to log lint messages."
  :type 'string
  :safe 'stringp
  :group 'pdf-linter)

(defcustom pdf-linter-jar nil
  "PDFBox preflight-app JAR."
  :group 'pdf-linter)

;;;###autoload
(defun pdf-lint ()
  "Run a PDF linter for the current buffer."
  (interactive)
  (when (get-buffer (pdf-linter--get-log-buffer))
    (kill-buffer (pdf-linter--get-log-buffer))) ;
  (let ((lint-command-to-run (pdf-linter--command-to-run buffer-file-name)))
    (pdf-linter--display-log)
    (apply 'start-process
           "PDF Linter"
           (pdf-linter--get-log-buffer)
           "java"
           lint-command-to-run)))

(defun pdf-linter--get-log-buffer ()
  "Return a log buffer for pdf-linter."
  (or (get-buffer pdf-linter-log-buffer)
      (with-current-buffer (get-buffer-create pdf-linter-log-buffer)
        (setq buffer-undo-list t)
        (nxml-mode)
        (current-buffer))))

(defun pdf-linter--display-log ()
  "Display the PDF lint log buffer."
  (let ((pop-up-windows t))
    (display-buffer (pdf-linter--get-log-buffer))
    (sit-for 0)))

(defun pdf-linter--command-to-run (pdf-file-path)
  "The PDFBox preflight-app command to run."
  (unless pdf-linter-jar
    (user-error "Error: Please set the pdf-linter-jar variable to the location of the PDFBox preflight-app JAR."))
  (unless (string-equal (downcase (file-name-extension pdf-file-path)) "pdf")
    (user-error "Error: Please run the command when the buffer is visiting a PDF file."))
  (list "-jar" (substitute-in-file-name pdf-linter-jar)
        "xml" pdf-file-path))

(provide 'pdf-linter)
;;; pdf-linter.el ends here
