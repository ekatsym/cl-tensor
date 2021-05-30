(defpackage cl-tensor.cuarray
  (:nicknames :clt.cuarray :clt.ca)
  (:use :common-lisp cl-tensor.util)
  (:export
    ;; Core
    #:*handle*
    #:*datatype*
    #:with-handle
    #:cuarray
    #:make-cuarray
    #:make-zeros-cuarray
    #:array->cuarray
    #:cuarray->array

    ;; BLAS level-1
    #:axpy
    #:asum
    #:copy
    #:dot
    #:nrm2
    #:scal
    #:amax
    #:amin

    ;; BLAS level-2
    #:gemv
    #:ger

    ;; BLAS level-3
    #:gemm

    ;; BLAS-like extensions
    #:geam))
(in-package :cl-tensor.cuarray)


;;; Core
(defparameter *handle* nil)
(defparameter *datatype* :float)

(defmacro with-handle ((&optional handle) &body body)
  (if handle
      `(let ((*handle* ,handle))
         (unwind-protect (multiple-value-prog1 (progn ,@body))
           (clt.cublas:destroy *handle*)))
      `(clt.cublas:with-handle *handle* ,@body)))

(defclass cuarray ()
  ((dimensions :initarg :dimensions :reader cuarray-dimensions)
   (datum :initarg :datum :reader cuarray-datum)))

(defmethod print-object ((object cuarray) stream)
  (format stream "#<CUARRAY :DIMENSIONS ~A :DATUM ~S>"
          (cuarray-dimensions object)
          (cuarray->array object)))

(defun make-cuarray (&rest dimensions)
  (make-instance 'cuarray
                 :dimensions dimensions
                 :datum      (clt.cublas:make-device-pointer
                               *datatype* (reduce #'* dimensions))))


(defun array->cuarray (array)
  (let* ((dims (array-dimensions array))
         (cuarr (apply #'make-cuarray (array-dimensions array)))
         (datum (cuarray-datum cuarr))
         (count (reduce #'* dims)))
    (cffi:with-foreign-object (carr *datatype* count)
      (dolist (index (indices dims))
        (let ((row-major-index (index->row-major-index index dims))
              (col-major-index (index->col-major-index index dims)))
          (setf (cffi:mem-aref carr *datatype* col-major-index)
                (coerce (row-major-aref array row-major-index)
                        (ecase *datatype*
                          (:float 'single-float)
                          (:double 'double-float))))))
      (clt.cublas:cuda-memcpy datum
                              carr
                              (* (cffi:foreign-type-size *datatype*) count)
                              :cuda-memcpy-default)
      (clt.cublas:cuda-device-synchronize)
      cuarr)))

(defun cuarray (x)
  (etypecase x
    (array (array->cuarray x))
    (number (array->cuarray (make-array '() :initial-element x)))))

(defun cuarray->array (cuarray)
  (let* ((datum (cuarray-datum cuarray))
         (dims (cuarray-dimensions cuarray))
         (arr (make-array dims))
         (count (reduce #'* dims)))
    (cffi:with-foreign-object (carr *datatype* count)
      (clt.cublas:cuda-memcpy carr
                              datum
                              (* (cffi:foreign-type-size *datatype*) count)
                              :cuda-memcpy-default)
      (clt.cublas:cuda-device-synchronize)
      (dotimes (i count)
        (setf (row-major-aref arr i)
              (cffi:mem-aref carr *datatype* i)))
      (dolist (index (indices dims))
        (let ((row-major-index (index->row-major-index index dims))
              (col-major-index (index->col-major-index index dims)))
          (setf (row-major-aref arr row-major-index)
                (cffi:mem-aref carr *datatype* col-major-index))))
      arr)))

(defun make-zeros-cuarray (&rest dimensions)
  (let ((cuarr (apply #'make-cuarray dimensions))
        (count (reduce #'* dimensions)))
    (cffi:with-foreign-object (=>x *datatype* count)
      (dotimes (i count)
        (setf (cffi:mem-aref =>x *datatype* i)
              (ecase *datatype*
                (:float 0.0f1)
                (:double 0.0d1))))
      (clt.cublas:cuda-memcpy (cuarray-datum cuarr)
                              =>x
                              count
                              :cuda-memcpy-default)
      cuarr)))

(define-condition cuarray-axis-unmatched-error (error)
  ((datum1 :initarg :datum1
           :reader cuarray-axis-unmatched-error-datum1)
   (datum1 :initarg :datum2
           :reader cuarray-axis-unmatched-error-datum2))
  (:report (lambda (o s)
             (format s "The dimensions~%~2T~D~%and~%~2T~D~%are different."
                     (cuarray-axis-unmatched-error-datum1 o)
                     (cuarray-axis-unmatched-error-datum2 o)))))

(defun gemm (alpha a b beta c &key trans-a? trans-b?)
  (assert (= (cuarray-dimension a 0) (cuarray-dimension c 0)) (a c)
          'cuarray-axis-unmatched-error
          :datum1 (first  (last (cuarray-dimensions a) 2))
          :datum2 (second (last (cuarray-dimensions c) 2)))
  (assert (= (cuarray-dimension b 1) (cuarray-dimension c 1)) (b c)
          'cuarray-axis-unmatched-error
          :datum1 (first  (last (cuarray-dimensions b) 2))
          :datum2 (second (last (cuarray-dimensions c) 2)))
  (assert (= (cuarray-dimension a 1) (cuarray-dimension b 0)) (a b)
          'cuarray-axis-unmatched-error
          :datum1 (first  (last (cuarray-dimensions a) 2))
          :datum2 (second (last (cuarray-dimensions b) 2)))
  (flet ((%gemm (&rest args)
           (ecase *datatype*
             (:float            (apply #'clt.cublas:sgemm args))
             (:double           (apply #'clt.cublas:dgemm args)))))
    (let* ((handle *handle*)
           (transa (ecase trans-a?
                     ((nil :cublas-op-n)          :cublas-op-n)
                     ((t :cublas-op-t) :cublas-op-t)))
           (transb (ecase trans-b?
                     ((nil :cublas-op-n)          :cublas-op-n)
                     ((t :cublas-op-t) :cublas-op-t)))
           (m (cuarray-dimension a 0))
           (n (cuarray-dimension b 1))
           (k (cuarray-dimension a 1))
           (lda m)
           (ldb k)
           (ldc m)
           (=>a (cuarray-datum a))
           (=>b (cuarray-datum b))
           (=>c (cuarray-datum c)))
      (cffi:with-foreign-objects ((=>alpha *datatype*)
                                  (=>beta  *datatype*))
        (setf (cffi:mem-ref =>alpha *datatype*) alpha)
        (setf (cffi:mem-ref =>beta  *datatype*) beta)
        (%gemm handle transa transb m n k =>alpha =>a lda =>b ldb =>beta =>c ldc)
        c))))
