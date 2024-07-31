(defpackage cl-airtable/tests/main
  (:use :cl
        :cl-airtable
        :rove))
(in-package :cl-airtable/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-airtable)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
