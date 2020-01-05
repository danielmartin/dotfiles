;;; gtest-mode-test.el --- Tests for `gtest-mode'            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Daniel Martín

;; This program is free software; you can redistribute it and/or modify
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

;; Tests for `gtest-mode'.

;;; Code:

(require 'ert)
(require 'gtest-mode)

(ert-deftest gtest-mode--parse-test-list-empty ()
  (should (equal (gtest-mode--test-list
                  "")
                 nil)))

(ert-deftest gtest-mode--parse-test-list-invalid ()
  (should-error (gtest-mode--test-list
                 "Garbage"))
  (should-error (gtest-mode--test-list
                 (concat "TestClass.\n"
                         " OnlyOneSpace")))
  (should-error (gtest-mode--test-list
                 (concat "TestClass\n"
                         "  NoDot"))))

(ert-deftest gtest-mode--parse-test-list-single-test ()
  (should (equal (gtest-mode--test-list
                  (concat "TestClass.\n"
                          "  FirstTest"))
                 '("TestClass.*"
                   "TestClass.FirstTest"))))

(ert-deftest gtest-mode--parse-test-list-two-test-cases ()
  (should (equal (gtest-mode--test-list
                  (concat "TestClass.\n"
                          "  FirstTest\n"
                          "AnotherTestClass.\n"
                          "  AnotherTest"))
                 '("TestClass.*"
                   "TestClass.FirstTest"
                   "AnotherTestClass.*"
                   "AnotherTestClass.AnotherTest"))))

(ert-deftest gtest-mode--parse-test-list-parameterized-cases ()
  (should (equal (gtest-mode--test-list
                  (concat "AnnotationManagerTests/DifferentDocuments.\n"
                          "  AddNoteAnnotation/0  # GetParam() ="))
                 '("AnnotationManagerTests/DifferentDocuments.*"
                   "AnnotationManagerTests/DifferentDocuments.AddNoteAnnotation/0"))))
