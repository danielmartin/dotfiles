;;; pbxproj-mode.el --- Major mode for editing Xcode project files -*- lexical-binding:t -*-

;; Copyright (C) 2017 Daniel Martín <mardani29@yahoo.es>
;;
;; Author: Daniel Martín <mardani29@yahoo.es>
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: Xcode

;;; Commentary:

;; This package adds support for basic edition of Xcode project files
;; (.pbxproj).

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defcustom pbxproj-mode-hook nil
  "Normal hook run when entering `pbxproj-mode'."
  :type 'hook
  :group 'pbxproj-mode)

;;;###autoload
(define-derived-mode pbxproj-mode text-mode "pbxproj"
  "Major mode for editing Xcode project files.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pbxproj\\'" . pbxproj-mode))

(provide 'pbxproj-mode)

;;; pbxproj-mode.el ends here
