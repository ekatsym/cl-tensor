(defpackage cl-tensor.blas.carray
  (:nicknames :clt.carray :clt.b.ca)
  (:use :common-lisp :cl-tensor.blas :cl-tensor.util :cl-tensor.blas.assert)
  (:export
    #:carray->array #:array->carray))
(in-package :cl-tensor.blas.carray)


;;; Core
(defclass carray ()
  ((dimensions :initarg :dimensions :reader carray-dimensions)
   (datum :initarg :datum :reader carray-datum)))

(defmethod print-object ((object carray) stream)
  (format stream "<CARRAY :DIMENSIONS ~A :DATUM ~S>"
          (carray-dimensions object)
          (carray->array object)))

(defun make-carray (dimensions &key displaced-to)
  (check-type dimensions list)
  (check-type displaced-to (or null carray))
  (mapc (lambda (dim) (check-type dim (integer 0 *))) dimensions)
  (make-instance 'carray
                 :dimensions dimensions
                 :datum      (if displaced-to
                                 (carray-datum displaced-to)
                                 (cffi:foreign-alloc
                                   (element-type) :count (reduce #'* dimensions)))))

(defun make-carray* (dimensions &key (init 0.0) (step #'1+) (key #'identity))
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim (integer 0 *))) dimensions)
  (check-type init real)
  (check-type step function)
  (check-type key function)
  (let* ((carr (make-carray dimensions))
         (total-size (blas-array-total-size carr))
         (datum (carray-datum carr)))
    (do ((i 0 (1+ i))
         (x init (funcall step x)))
        ((>= i total-size) nil)
        (setf (cffi:mem-aref datum (element-type) i)
              (coerce (funcall key x)
                      (ecase (element-type)
                        (:float 'single-float)
                        (:double 'double-float)))))
    carr))

(defun array->carray (array)
  (check-type array array)
  (let* ((dims (array-dimensions array))
         (carr (make-carray dims))
         (datum (carray-datum carr))
         (count (array-total-size array)))
    (dotimes (i count)
      (setf (cffi:mem-aref datum (element-type) i)
            (coerce (row-major-aref array i)
                    (ecase (element-type)
                      (:float 'single-float)
                      (:double 'double-float)))))
    carr))

(defun carray->array (carray)
  (check-type carray carray)
  (let* ((datum (carray-datum carray))
         (dims (carray-dimensions carray))
         (arr (make-array dims))
         (count (reduce #'* dims)))
    (dotimes (i count)
      (setf (row-major-aref arr i)
            (cffi:mem-aref datum (element-type) i)))
    arr))


;;; Element Type
(defun element-type ()
  (check-type *element-type* element-type)
  (ecase *element-type*
    (s :float)
    (d :double)))


;;; Constructor
(defmethod make-blas-array ((blas-name (eql 'cblas)) dimensions)
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim (integer 0 *))) dimensions)
  (make-carray dimensions))

(defmethod make-blas-array* ((blas-name (eql 'cblas)) dimensions
                             &key (init 0.0) (step #'1+) (key #'identity))
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim (integer 0 *))) dimensions)
  (check-type init number)
  (check-type step function)
  (check-type key function)
  (make-carray* dimensions))


;;; Accessor
(defmethod blas-array-dimensions ((blas-array carray))
  (carray-dimensions blas-array))


;;; Converter
(defmethod coerce-blas-array ((blas-array carray) (output-blas-name (eql 'list)))
  (carray->array blas-array))

(defmethod coerce-blas-array ((blas-array array) (output-blas-name (eql 'cblas)))
  (array->carray blas-array))


;;; Utility
(defmethod trans ((a carray))
  (assert-rank a 2)
  (let* ((dims (carray-dimensions a))
         (m (first dims))
         (n (second dims))
         (b (make-blas-array 'cblas (reverse dims)))
         (=>a (carray-datum a))
         (=>b (carray-datum b)))
    (dotimes (i m)
      (dotimes (j n)
        (setf (cffi:mem-aref =>b (element-type) (+ (* j m) i))
              (cffi:mem-aref =>a (element-type) (+ (* i n) j)))))
    b))

(defmethod diag ((x carray))
  (assert-rank x 1)
  (let* ((n (blas-array-dimension x 0))
         (y (make-blas-array 'cblas (list n n))))
    (copy x y :stride-y (1+ n))))

(defmethod reshape (dimensions (a carray))
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim (integer 0 *))) dimensions)
  (assert (= (reduce #'* dimensions) (blas-array-total-size a))
          (dimensions a))
  (make-carray dimensions :displaced-to a))
