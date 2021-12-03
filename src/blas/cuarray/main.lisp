(defpackage cl-tensor.blas.cuarray.main
  (:use :common-lisp :cl-tensor.core :cl-tensor.util
        :cl-tensor.blas.cuarray :cl-tensor.blas.util))
(in-package :cl-tensor.blas.cuarray.main)


;;; Utility
(defun element-type->cffi-type (element-type)
  (assert (subtypep element-type 'element-type) (element-type))
  (ecase element-type
    (single-float :float)
    (short-float :float)
    (double-float :double)
    (long-float :double)))

;;; Constructors
(defmethod make-blas-array ((array-type-spec (eql 'cuarray)) dimensions
                             &key (element-type 'single-float)
                                  initial-element
                                  displaced-to)
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim index)) dimensions)
  (assert (subtypep element-type 'element-type) (element-type))
  (assert (typep initial-element `(or null ,element-type)) (initial-element))
  (check-type displaced-to (or null cuarray))
  (assert (not (and displaced-to initial-element)) (initial-element displaced-to))
  (let ((cffi-type (element-type->cffi-type element-type))
        (total-size (reduce #'* dimensions)))
    (make-instance
      'cuarray
      :dimensions dimensions
      :datum      (cond (displaced-to    (clt.blas::cuarray-datum displaced-to))
                        (initial-element (let ((=>a/d (clt.cublas:make-device-pointer
                                                        cffi-type total-size)))
                                           (cffi:with-foreign-object (=>a/h cffi-type total-size)
                                             (dotimes (i total-size)
                                               (setf (cffi:mem-ref =>a/h cffi-type i)
                                                     (coerce initial-element element-type)))
                                             (clt.cublas:cuda-memcpy =>a/d =>a/h total-size :cuda-memcpy-default)
                                             =>a/d)))
                        (t               (clt.cublas:make-device-pointer cffi-type total-size))))))

(defmethod make-blas-array* ((array-type-spec (eql 'cuarray)) dimensions
                             &key (element-type 'single-float)
                                  (seed 0)
                                  (next #'identity)
                                  (key #'identity))
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim index)) dimensions)
  (assert (subtypep element-type 'element-type) (element-type))
  (assert (typep seed `(or null ,element-type)) (seed))
  (check-type next function)
  (check-type key function)
  (let* ((cffi-type (element-type->cffi-type element-type))
         (total-size (reduce #'* dimensions))
         (=>a/d (clt.cublas:make-device-pointer cffi-type total-size)))
    (make-instance
      'cuarray
      :dimensions dimensions
      :datum      (cffi:with-foreign-object (=>a/h cffi-type total-size)
                    (do ((i 0 (1+ i))
                         (x seed (funcall next x)))
                        ((>= i total-size))
                        (setf (cffi:mem-ref =>a/h cffi-type i)
                              (coerce (funcall key x) element-type)))
                    (clt.cublas:cuda-memcpy =>a/d =>a/h total-size :cuda-memcpy-default)
                    =>a/d))))

;;; Accessors
(defmethod blas-array-dimensions ((blas-array cuarray))
  (clt.blas::cuarray-dimensions blas-array))

;;; Utilities
#|
(defmethod trans ((a cuarray))
  (assert-rank a 2)
  (clt.cublas:sgeam )
  )
(clt.cublas:create-handle)

(clt.cublas:make-device-pointer )

(type-of (clt.blas::cuarray-datum (make-blas-array 'cuarray '(3 2))))
(class-of (clt.blas::cuarray-datum (make-blas-array 'cuarray '(3 2))))
|#
