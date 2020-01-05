;;; gtest-mode.el --- Minor mode to manage Google Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Daniel Martín

;; Author: Daniel Martín <mardani29@yahoo.es>
;; URL: https://github.com/danielmartin/gtest-mode
;; Keywords: programming
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode to manage Google Tests.  Among the supported features are:
;;
;; - Run individual test cases when the point is inside one of them.
;;
;; - List all tests in a target and run any of them (regular
;; expression filters are also supported).

;;; Code:

(require 'compile)

(defgroup gtest nil
  "gtest group"
  :group 'tools)

(defcustom gtest-mode-target ""
  "Binary target that contains the Google Tests to run."
  :group 'tools
  :type 'string)

(defun gtest-mode--extract-test-name ()
  "Extract a Google Test name around point.
A Google Test name has the format CLASS.TEST-NAME."
  (save-excursion
    (beginning-of-defun)
    (if (looking-at "TEST.*(\\(.*\\),[ ]*\\(.*\\))")
        (progn
          (let ((class-name (match-string 1))
                (test-name (match-string 2)))
            (concat class-name "." test-name)))
      (user-error "No Google Test found around point"))))

(defun gtest-mode--run-test-filter (test-filter target)
  "Run Google Test filter TEST-FILTER on TARGET."
  (message "Running %s" test-filter)
  (compile (concat target " --gtest_filter=" test-filter)))

(defun gtest-mode--repeat-test-filter (test-filter target times)
  "Run a Google Test filter TEST-FILTER on TARGET TIMES times."
  (message "Repeating %s %s times" test-filter times)
  (compile (concat target " --gtest_filter=" test-filter
                   " --gtest_repeat=" (number-to-string times))))

(defun gtest-mode-run-at-point (arg)
  "Run the Google Test around point.
With prefix argument, repeat the test ARG times."
  (interactive "P")
  (if (and arg (> arg 0))
      (gtest-mode--repeat-test-filter
       (gtest-mode--extract-test-name) gtest-mode-target arg)
    (gtest-mode--run-test-filter (gtest-mode--extract-test-name) gtest-mode-target)))

(defun gtest-mode--test-list (test-output)
  "Return a list of test cases from TEST-OUTPUT.
TEST-OUTPUT has the following format:
TestClass.
  FirstTest
  SecondTest
AnotherTestClass.
  Test1
  Test2"
  (with-temp-buffer
    (insert test-output)
    (goto-char (point-min))
    (let ((test-commands)
          (current-test-class))
      (while (not (eobp))
        (cond ((looking-at "\\(^\\w+/?\\w+\\)\\(\\.\\)")
               ;; A new test class is inserted with the "*" test
               ;; case. This lets users run all tests from a test
               ;; class.
               (setq current-test-class (match-string 1))
               (push (concat current-test-class ".*")
                     test-commands))
              ((looking-at "  \\(\\w+\\(/[0-9]+\\)?\\)")        ;This is a test case
               (push (concat current-test-class "." (match-string 1))
                     test-commands))
              (t (user-error "The test output does not have the expected format %s"
                             (thing-at-point 'line t))))
        (forward-line 1))
      (nreverse test-commands))))

(defun gtest-mode--completing-read (collection)
  "Call `completing-read' with COLLECTION."
  (completing-read "Test to run: " collection))

(defun gtest-mode-run ()
  "Show a list of test cases contained in `gtest-mode-target' and run any of them."
  (interactive)
  (let ((target gtest-mode-target))
    (with-temp-buffer
      (insert
       (shell-command-to-string (concat target " --gtest_list_tests")))
      ;; Google Test output may contain program-specific output before
      ;; the actual list of tests, so search for the first thing that
      ;; looks like a test class and process from there to the end of
      ;; the command output.
      (goto-char (point-min))
      (if (re-search-forward "\\(^\\w+/?\\w+\\)\\(\\.\\)")
          (gtest-mode--run-test-filter
           (gtest-mode--completing-read
            (gtest-mode--test-list
             (buffer-substring (match-beginning 0) (point-max))))
           target)
        (user-error "No Google Tests found in the `gtest-mode-target' target")))))

(define-minor-mode gtest-mode
  "Minor mode to run Google Tests."
  :keymap (let ((gtest-mode-map (make-sparse-keymap)))
	    (define-key gtest-mode-map (kbd "C-c l") 'gtest-mode-run-at-point)
            (define-key gtest-mode-map (kbd "C-c r") 'gtest-mode-run)
	    gtest-mode-map))

(provide 'gtest-mode)
;;; gtest-mode.el ends here
