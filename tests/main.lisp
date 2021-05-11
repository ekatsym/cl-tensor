(defpackage cl-tensor/tests/main
  (:use :cl
        :cl-tensor
        :rove))
(in-package :cl-tensor/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-tensor)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
