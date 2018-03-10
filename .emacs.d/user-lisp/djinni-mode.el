;;; djinni-mode.el --- Major-mode for editing Djinni files. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Daniel Martín

;; Author: Daniel Martín <mardani29@yahoo.es>
;; Version: 1.0
;; Package-Requires: ((emacs "24.4") (seq "2.3"))
;; Keywords: djinni

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major-mode for editing Djinni files (https://github.com/dropbox/djinni).

;;; Code:

(defvar djinni-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Parenthesis
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\} "){" st)
    st)
  "Syntax table in use in `djinni-mode' buffers.")

(defvar djinni-font-lock-keywords
  (let* (;; Define categories of keywords.
         (djinni-keywords '("interface" "record" "enum" "flags" "deriving" "eq" "ord" "const" "static"))
         (djinni-types '("bool" "string" "i8" "i16" "i32" "i64" "f8" "f16" "f32" "f64"
                         "binary" "date" "list" "set" "map" "optional"))

         ;; Generate an optimized regexp for each category.
         (djinni-keywords-regexp (regexp-opt djinni-keywords 'words))
         (djinni-types-regexp (regexp-opt djinni-types 'words))
         )
    `(
      ("\\([a-zA-Z0-9_]+\\)\\s-*:\\s-*\\([a-zA-Z0-9_]+\\)" (1 'font-lock-variable-name-face) (2 'font-lock-type-face))
      ("^\\s-*\\([a-zA-Z0-9_]+\\)\\s-*;" . (1 'font-lock-variable-name-face))
      ("+\\(j\\|c\\|o\\)\\b" . font-lock-keyword-face)
      ;; @import is also considered a keyword.
      ("@\\import\\b" . font-lock-keyword-face)
      (")\\s-*:\\s-*\\([a-zA-Z0-9_]+\\)" . (1 'font-lock-type-face))
      ("<\\s-*\\([a-zA-Z0-9_]+\\)\\s-*>" . (1 'font-lock-type-face))
      ("^\\s-*\\(\\sw+\\)\\s-*=" . (1 'font-lock-type-face))
      ("^\\s-*\\(static\\|const\\s-*\\)?\\s-+\\(\\(\\sw\\|\\s_\\)+\\)\\s-*(" . (2 'font-lock-function-name-face))
      (,djinni-types-regexp . font-lock-type-face)
      (,djinni-keywords-regexp . font-lock-keyword-face))))

(defun djinni-font-lock-setup ()
  (setq font-lock-defaults
        '((djinni-font-lock-keywords))))

;;;###autoload
(define-derived-mode djinni-mode prog-mode "Djinni"
  "Major mode for editing Djinni files."
  ;; Syntax table
  (set-syntax-table djinni-syntax-table)
  ;; Comments
  (setq-local comment-start "#")
  ;; Font locking
  (djinni-font-lock-setup)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.djinni\\'" . djinni-mode))

(provide 'djinni-mode)
;;; djinni-mode.el ends here
