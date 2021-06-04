(defpackage cl-tensor.cuarray
  (:nicknames :clt.cuarray :clt.ca)
  (:use :common-lisp cl-tensor.util)
  (:export
    ;; Core
    #:*handle*
    #:*datatype*
    #:with-handle
    #:cuarray

    ;; Constructor
    #:make-cuarray
    #:make-zeros-cuarray
    #:make-custom-cuarray

    ;; Accessor
    #:cuarray-rank
    #:cuarray-dimension
    #:cuarray-total-size

    ;; Converter
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
      `(let ((*handle* ,handle)) (progn ,@body))
      `(clt.cublas:with-handle *handle* ,@body)))

(defclass cuarray ()
  ((dimensions :initarg :dimensions :reader cuarray-dimensions)
   (datum :initarg :datum :reader cuarray-datum)))

(defmethod print-object ((object cuarray) stream)
  (format stream "#<CUARRAY :DIMENSIONS ~A :DATUM ~S>"
          (cuarray-dimensions object)
          (cuarray->array object)))


;;; Constructors
(defun make-cuarray (dimensions &key displaced-to)
  (make-instance 'cuarray
                 :dimensions dimensions
                 :datum      (if displaced-to
                                 (cuarray-datum displaced-to)
                                 (clt.cublas:make-device-pointer
                                   *datatype* (reduce #'* dimensions)))))

(defun make-zeros-cuarray (dimensions)
  (let* ((cuarr (make-cuarray dimensions))
         (total-size (cuarray-total-size cuarr)))
    (cffi:with-foreign-object (=>x *datatype* total-size)
      (dotimes (i total-size)
        (setf (cffi:mem-aref =>x *datatype* i)
              (ecase *datatype*
                (:float 0.0f1)
                (:double 0.0d1))))
      (clt.cublas:cuda-memcpy (cuarray-datum cuarr)
                              =>x
                              (* (cffi:foreign-type-size *datatype*) total-size)
                              :cuda-memcpy-default)
      cuarr)))

(defun make-custom-cuarray (dimensions &key (init 0.0) (step #'1+) (key #'identity))
  (let* ((cuarr (make-cuarray dimensions))
         (total-size (cuarray-total-size cuarr)))
    (cffi:with-foreign-object (=>a *datatype* total-size)
      (do ((indices (indices dimensions) (rest indices))
           (x init (funcall step x)))
          ((endp indices) nil)
          (let ((index (first indices)))
            (setf (cffi:mem-aref =>a *datatype* (index->col-major-index index dimensions))
                  (coerce (funcall key x)
                          (ecase *datatype*
                            (:float  'single-float)
                            (:double 'double-float))))))
      (clt.cublas:cuda-memcpy (cuarray-datum cuarr) =>a
                              (* (cffi:foreign-type-size *datatype*) total-size)
                              :cuda-memcpy-default)
      cuarr)))


;;; Accessors
(defun cuarray-rank (cuarray)
  (check-type cuarray cuarray)
  (length (cuarray-dimensions cuarray)))

(defun cuarray-dimension (cuarray axis-number)
  (check-type cuarray cuarray)
  (nth axis-number (cuarray-dimensions cuarray)))

(defun cuarray-total-size (cuarray)
  (check-type cuarray cuarray)
  (reduce #'* (cuarray-dimensions cuarray)))


;;; Convertors
(defun array->cuarray (array)
  (let* ((dims (array-dimensions array))
         (cuarr (make-cuarray (array-dimensions array)))
         (datum (cuarray-datum cuarr))
         (count (reduce #'* dims)))
    (cffi:with-foreign-object (carr *datatype* count)
      (dolist (index (indices dims))
        (let ((row-major-index (index->row-major-index index dims))
              (col-major-index (index->col-major-index index dims)))
          (setf (cffi:mem-aref carr *datatype* col-major-index)
                (coerce (row-major-aref array row-major-index)
                        (ecase *datatype*
                          (:float  'single-float)
                          (:double 'double-float))))))
      (clt.cublas:cuda-memcpy datum
                              carr
                              (* (cffi:foreign-type-size *datatype*) count)
                              :cuda-memcpy-default)
      cuarr)))

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
      (dolist (index (indices dims))
        (let ((row-major-index (index->row-major-index index dims))
              (col-major-index (index->col-major-index index dims)))
          (setf (row-major-aref arr row-major-index)
                (cffi:mem-aref carr *datatype* col-major-index))))
      arr)))

(defun cuarray (x)
  (etypecase x
    (array (array->cuarray x))
    (number (array->cuarray (make-array '() :initial-element x)))))


;;; BLAS functions
(defun axpy (alpha x y)
  (assert (= (cuarray-rank x) (cuarray-rank y) 1) (x y))
  (let ((x-len (cuarray-dimension x 0))
        (y-len (cuarray-dimension y 0)))
    (assert (= x-len y-len) (x y)
            'cuarray-dimension-unmatched-error
            :datum1 x-len
            :datum2 y-len)
    (flet ((%axpy (&rest args)
             (ecase *datatype*
               (:float  (apply #'clt.cublas:saxpy args))
               (:double (apply #'clt.cublas:daxpy args)))))
      (let ((handle *handle*)
            (n x-len)
            (=>x (cuarray-datum x))
            (incx 1)
            (=>y (cuarray-datum y))
            (incy 1))
        (cffi:with-foreign-object (=>alpha *datatype*)
          (setf (cffi:mem-ref =>alpha *datatype*)
                (coerce alpha (ecase *datatype*
                                (:float  'single-float)
                                (:double 'double-float))))
          (%axpy handle n =>alpha =>x incx =>y incy)
          y)))))

(defun gemm (alpha a b beta c &key trans-a? trans-b?)
  (assert (= (cuarray-rank a) (cuarray-rank b) (cuarray-rank c) 2) (a b c))
  (let ((a-nrow (cuarray-dimension a 0))
        (a-ncol (cuarray-dimension a 1))
        (b-nrow (cuarray-dimension b 0))
        (b-ncol (cuarray-dimension b 1))
        (c-nrow (cuarray-dimension c 0))
        (c-ncol (cuarray-dimension c 1)))
    (assert (= a-nrow c-nrow) (a c)
            'cuarray-dimension-unmatched-error
            :datum1 a-nrow
            :datum2 c-nrow)
    (assert (= b-ncol c-ncol) (b c)
            'cuarray-dimension-unmatched-error
            :datum1 b-ncol
            :datum2 c-ncol)
    (assert (= a-ncol b-nrow) (a b)
            'cuarray-dimension-unmatched-error
            :datum1 a-ncol
            :datum2 b-nrow)
    (flet ((%gemm (&rest args)
             (ecase *datatype*
               (:float            (apply #'clt.cublas:sgemm args))
               (:double           (apply #'clt.cublas:dgemm args)))))
      (let* ((handle *handle*)
             (transa (ecase trans-a?
                       ((nil :cublas-op-n) :cublas-op-n)
                       ((t :cublas-op-t)   :cublas-op-t)))
             (transb (ecase trans-b?
                       ((nil :cublas-op-n) :cublas-op-n)
                       ((t :cublas-op-t)   :cublas-op-t)))
             (m c-nrow)
             (n c-ncol)
             (k a-ncol)
             (lda m)
             (ldb k)
             (ldc m)
             (=>a (cuarray-datum a))
             (=>b (cuarray-datum b))
             (=>c (cuarray-datum c)))
        (cffi:with-foreign-objects ((=>alpha *datatype*)
                                    (=>beta  *datatype*))
          (setf (cffi:mem-ref =>alpha *datatype*)
                (coerce alpha
                        (ecase *datatype*
                          (:float 'single-float)
                          (:double 'double-float))))
          (setf (cffi:mem-ref =>beta  *datatype*)
                (coerce beta
                        (ecase *datatype*
                          (:float 'single-float)
                          (:double 'double-float))))
          (%gemm handle transa transb m n k =>alpha =>a lda =>b ldb =>beta =>c ldc)
          c)))))

(define-condition cuarray-dimension-unmatched-error (error)
  ((datum1 :initarg :datum1
           :reader cuarray-dimension-unmatched-error-datum1)
   (datum1 :initarg :datum2
           :reader cuarray-dimension-unmatched-error-datum2))
  (:report (lambda (o s)
             (format s "The dimensions~%~2T~D~%and~%~2T~D~%are different."
                     (cuarray-dimension-unmatched-error-datum1 o)
                     (cuarray-dimension-unmatched-error-datum2 o)))))
