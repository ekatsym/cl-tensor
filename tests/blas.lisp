(defpackage cl-tensor/tests.blas
  (:use :common-lisp :cl-tensor.blas)
  (:import-from :rove #:deftest #:testing #:ok #:ng))
(in-package :cl-tensor/tests.blas)


(defun random-element-type ()
  (nth (random 4)
       '(single-float double-float (complex single-float) (complex double-float))))

(defun random-scalar (etype)
  (case etype
    ((single-float double-float)
     (coerce (- (random 2.0d6) 1.0d6) etype))
    (otherwise
      (coerce (complex (- (random 2.0d6) 1.0d6)
                       (- (random 2.0d6) 1.0d6))
              etype))))

(defun random-array (etype dims)
  (let ((arr (make-blas-array 'array dims :element-type etype)))
    (dotimes (i (array-total-size arr))
      (setf (row-major-aref arr i)
            (random-scalar etype)))
    arr))

(defun random-array* ()
  (let* ((etype (random-element-type))
         (rank (random 5))
         (dims (loop repeat rank collect (random 100))))
    (random-array etype dims)))

(defun array= (a1 a2)
  (declare (optimize (space 3)))
  (and (equal (array-dimensions a1)
              (array-dimensions a2))
       (do ((n (array-total-size a1))
            (i 0 (1+ i))
            (b t (and b (= (row-major-aref a1 i) (row-major-aref a2 i)))))
           ((or (>= i n) (not b)) b))))

(deftest array-test
  (testing "trans"
    (dotimes (i 100)
      (let* ((m (random 100))
             (n (random 100))
             (etype (random-element-type))
             (a (random-array etype (list m n)))
             (ta (trans a)))
        (ok (equal (blas-array-dimensions ta) (list n m)))
        (ok (array= (trans ta) a)))))
  (testing "copy"
    (dotimes (i 100)
      (let* ((etype (random-element-type))
             (rank (random 5))
             (dims (loop repeat rank collect (random 50)))
             (x (random-array etype dims))
             (y (make-blas-array 'array dims :element-type etype)))
        (copy x y)
        (ok (array= x y)))))
  (testing "axpy"
    (dotimes (i 100)
      (let* ((etype (random-element-type))
             (rank (random 5))
             (dims (loop repeat rank collect (random 50)))
             (alpha (coerce 1.0 etype))
             (x (random-array etype dims))
             (y (make-blas-array 'array dims :element-type etype :initial-element 0)))
        (axpy alpha x y)
        (ok (array= x y))))))
