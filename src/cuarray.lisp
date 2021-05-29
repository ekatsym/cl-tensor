(defpackage cl-tensor.cuarray
  (:nicknames :clt.cuarray :clt.ca)
  (:use :common-lisp)
  (:export
    ;; Core
    #:blas-array
    #:*handle*
    #:*datatype*

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

(defun make-cuarray (&rest dimensions)
  (make-instance 'cuarray
                 :dimensions dimensions
                 :datum      (clt.cublas:make-device-pointer
                               *datatype* (reduce #'* dimensions))))

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
                              :cuda-memcpy-host-to-device)
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
