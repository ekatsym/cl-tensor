(defpackage cl-tensor.blas.cuarray
  (:nicknames :clt.cuarray :clt.b.cu)
  (:use :common-lisp :cl-tensor.blas :cl-tensor.util :cl-tensor.blas.assert)
  (:import-from cl-tensor.blas.cublas #:create-handle #:destroy)
  (:export
    #:cuarray->array #:array->cuarray))
(in-package :cl-tensor.blas.cuarray)


;;; Core
(defclass cuarray ()
  ((dimensions :initarg :dimensions :reader cuarray-dimensions)
   (datum :initarg :datum :reader cuarray-datum))) 

(defmethod print-object ((object cuarray) stream)
  (format stream "#<CUARRAY :DIMENSIONS ~A :DATUM ~S>"
          (cuarray-dimensions object)
          (cuarray->array object)))

(defun make-cuarray (dimensions &key displaced-to)
  (check-type dimensions list)
  (check-type displaced-to (or null cuarray))
  (mapc (lambda (dim) (check-type dim (integer 0 *))) dimensions)
  (make-instance 'cuarray
                 :dimensions dimensions
                 :datum      (if displaced-to
                                 (cuarray-datum displaced-to)
                                 (clt.cublas:make-device-pointer
                                   (element-type) (reduce #'* dimensions)))))

(defun make-cuarray* (dimensions &key (init 0.0) (step #'1+) (key #'identity))
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim (integer 0 *))) dimensions)
  (check-type init real)
  (check-type step function)
  (check-type key function)
  (let* ((cuarr (make-cuarray dimensions))
         (total-size (blas-array-total-size cuarr)))
    (cffi:with-foreign-object (=>a (element-type) total-size)
      (do ((i 0 (1+ i))
           (x init (funcall step x)))
          ((>= i total-size) nil)
          (setf (cffi:mem-aref =>a (element-type) i)
                (coerce (funcall key x)
                        (ecase (element-type)
                          (:float  'single-float)
                          (:double 'double-float)))))
      (clt.cublas:cuda-memcpy (cuarray-datum cuarr) =>a
                              (* (cffi:foreign-type-size (element-type)) total-size)
                              :cuda-memcpy-default)
      cuarr)))

(defun array->cuarray (array)
  (check-type array array)
  (let* ((dims (array-dimensions array))
         (cuarr (make-cuarray dims))
         (datum (cuarray-datum cuarr))
         (count (array-total-size array)))
    (cffi:with-foreign-object (carr (element-type) count)
      (dotimes (i count)
        (setf (cffi:mem-aref carr (element-type) i)
              (coerce (row-major-aref array i)
                      (ecase (element-type)
                        (:float  'single-float)
                        (:double 'double-float)))))
      (clt.cublas:cuda-memcpy datum
                              carr
                              (* (cffi:foreign-type-size (element-type)) count)
                              :cuda-memcpy-default)
      cuarr)))

(defun cuarray->array (cuarray)
  (check-type cuarray cuarray)
  (let* ((datum (cuarray-datum cuarray))
         (dims (cuarray-dimensions cuarray))
         (arr (make-array dims))
         (count (reduce #'* dims)))
    (cffi:with-foreign-object (carr (element-type) count)
      (clt.cublas:cuda-memcpy carr
                              datum
                              (* (cffi:foreign-type-size (element-type)) count)
                              :cuda-memcpy-default)
      (dotimes (i count)
        (setf (row-major-aref arr i)
              (cffi:mem-aref carr (element-type) i)))
      arr)))

;;; State
(defmethod start-blas ((blas-name (eql 'cublas)))
  (create-handle))

(defmethod finish-blas ((blas-name (eql 'cublas)) state)
  (destroy state))


;;; Element Type
(defun element-type ()
  (check-type *element-type* element-type)
  (ecase *element-type*
    (s :float)
    (d :double)))


;;; Constructor
(defmethod make-blas-array ((blas-name (eql 'cublas)) dimensions)
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim (integer 0 *))) dimensions)
  (make-cuarray dimensions))

(defmethod make-blas-array* ((blas-name (eql 'cublas)) dimensions
                             &key (init 0.0) (step #'1+) (key #'identity))
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim (integer 0 *))) dimensions)
  (check-type init number)
  (check-type step function)
  (check-type key function)
  (make-cuarray* dimensions :init init :step step :key key))

(defmethod make-blas-array-with-template ((template cuarray))
  (make-blas-array 'cublas (blas-array-dimensions template)))


;;; Accessor
(defmethod blas-array-dimensions ((blas-array cuarray))
  (cuarray-dimensions blas-array))


;;; Converter
(defmethod coerce-blas-array ((blas-array cuarray) (output-blas-name (eql 'lisp)))
  (cuarray->array blas-array))

(defmethod coerce-blas-array ((blas-array array) (output-blas-name (eql 'cublas)))
  (array->cuarray blas-array))


;;; Utility
(defmethod trans ((a cuarray))
  (assert-rank a 2)
  (let ((c (make-blas-array 'cublas (reverse (blas-array-dimensions a)))))
    (geam 1 a 0 c c :trans-a? t)))

(defmethod diag ((x cuarray))
  (assert-rank x 1)
  (let* ((n (blas-array-dimension x 0))
         (y (make-blas-array 'cublas (list n n))))
    (copy x y :stride-y (1+ n))))

(defmethod reshape (dimensions (a cuarray))
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim (integer 0 *))) dimensions)
  (assert (= (reduce #'* dimensions) (blas-array-total-size a))
          (dimensions a))
  (make-cuarray dimensions :displaced-to a))

(defun geam (alpha a beta b c &key trans-a? trans-b?)
  (check-type alpha real) (check-type a cuarray)
  (check-type beta real) (check-type b cuarray)
  (check-type c cuarray)
  (assert-rank a 2) (assert-rank b 2) (assert-rank c 2)
  (if (not trans-a?)
      (progn (assert-dimension-match a 0 c 0)
             (assert-dimension-match a 1 c 1))
      (progn (assert-dimension-match a 1 c 0)
             (assert-dimension-match a 0 c 1)))
  (if (not trans-b?)
      (progn (assert-dimension-match b 0 c 0)
             (assert-dimension-match b 1 c 1))
      (progn (assert-dimension-match b 1 c 0)
             (assert-dimension-match b 0 c 1)))
  (flet ((%geam (&rest args)
           (ecase (element-type)
             (:float  (apply #'clt.cublas:sgeam args))
             (:double (apply #'clt.cublas:dgeam args)))))
    (with-blas (cublas *blas-state*)
      (cffi:with-foreign-objects ((=>alpha (element-type))
                                  (=>beta (element-type)))
        (setf (cffi:mem-ref =>alpha (element-type))
              (coerce alpha (ecase (element-type)
                              (:float  'single-float)
                              (:double 'double-float))))
        (setf (cffi:mem-ref =>beta (element-type))
              (coerce beta (ecase (element-type)
                             (:float  'single-float)
                             (:double 'double-float))))
        (%geam
          *blas-state*
          (if trans-a? :cublas-op-t :cublas-op-n)
          (if trans-b? :cublas-op-t :cublas-op-n)
          (blas-array-dimension c 0)
          (blas-array-dimension c 1)
          =>alpha
          (cuarray-datum a) (blas-array-dimension a 0)
          =>beta
          (cuarray-datum b) (blas-array-dimension b 0)
          (cuarray-datum c) (blas-array-dimension c 0))
        c))))


;;; BLAS
(defmacro with-cublas-function (cublas-function &body body)
  (flet ((symbolicate (&rest things)
           (intern
             (coerce
               (reduce (lambda (thing acc)
                         (append (coerce (string thing) 'list) acc))
                       things
                       :initial-value '()
                       :from-end t)
               'string)
             :cl-tensor.blas.cublas)))
    (let ((sf (symbolicate 's (subseq (string cublas-function) 1)))
          (df (symbolicate 'd (subseq (string cublas-function) 1)))
          (g!args (gensym "ARGS")))
      `(flet ((,cublas-function (&rest ,g!args)
                (ecase (element-type)
                  (:float  (apply (function ,sf) ,g!args))
                  (:double (apply (function ,df) ,g!args)))))
         ,@body))))

;; BLAS Level-1
(defmethod scal (alpha (x cuarray))
  (check-type alpha number)
  (with-cublas-function ?scal
    (with-blas (cublas *blas-state*)
      (cffi:with-foreign-object (=>alpha (element-type))
        (setf (cffi:mem-ref =>alpha (element-type))
              (coerce alpha (ecase (element-type)
                              (:float  'single-float)
                              (:double 'double-float))))
        (?scal *blas-state*
               (blas-array-total-size x)
               =>alpha
               (cuarray-datum x)
               1)
        x))))

(defmethod copy ((x cuarray) (y cuarray) &key count (stride-x 1) (stride-y 1))
  (check-type count (or null (integer 0 *)))
  (check-type stride-x (integer 1 *))
  (check-type stride-y (integer 1 *))
  (if count
      (assert (and (<= count (ceiling (blas-array-total-size x) stride-x))
                   (<= count (ceiling (blas-array-total-size y) stride-y)))
              (x y count))
      (assert (<= (ceiling (blas-array-total-size x) stride-x)
                  (ceiling (blas-array-total-size y) stride-y))
              (x y)))
  (flet ((%copy (&rest args)
           (ecase (element-type)
             (:float  (apply #'clt.cublas:scopy args))
             (:double (apply #'clt.cublas:dcopy args)))))
    (let ((count (or count (blas-array-total-size x))))
      (with-blas (cublas *blas-state*)
        (%copy *blas-state*
               count
               (cuarray-datum x)
               stride-x
               (cuarray-datum y)
               stride-y)
        y))))

(defmethod axpy (alpha (x cuarray) (y cuarray))
  (check-type alpha number)
  (assert-total-size-match x y)
  (with-cublas-function ?axpy
    (with-blas (cublas *blas-state*)
      (cffi:with-foreign-object (=>alpha (element-type))
        (setf (cffi:mem-ref =>alpha (element-type))
              (coerce alpha (ecase (element-type)
                              (:float  'single-float)
                              (:double 'double-float))))
        (?axpy *blas-state*
               (blas-array-total-size x)
               =>alpha
               (cuarray-datum x) 1
               (cuarray-datum y) 1)
        y))))

(defmethod dot ((x cuarray) (y cuarray))
  (assert-dimensions-match x y)
  (with-cublas-function ?dot
    (with-blas (cublas *blas-state*)
      (cffi:with-foreign-object (=>result (element-type))
        (?dot *blas-state*
              (blas-array-total-size x)
              (cuarray-datum x) 1
              (cuarray-datum y) 1
              =>result)
        (cffi:mem-ref =>result (element-type))))))

(defmethod nrm2 ((x cuarray))
  (with-cublas-function ?nrm2
    (with-blas (cublas *blas-state*)
      (cffi:with-foreign-object (=>result (element-type))
        (?nrm2 *blas-state*
               (blas-array-total-size x)
               (cuarray-datum x)
               1
               =>result)
        (cffi:mem-ref =>result (element-type))))))

(defmethod asum ((x cuarray))
  (with-cublas-function ?asum
    (with-blas (cublas *blas-state*)
      (cffi:with-foreign-object (=>result (element-type))
        (?asum *blas-state*
               (blas-array-total-size x)
               (cuarray-datum x)
               1
               =>result)
        (cffi:mem-ref =>result (element-type))))))

(defmethod amax ((x cuarray))
  (with-cublas-function ?amax
    (with-blas (cublas *blas-state*)
      (cffi:with-foreign-object (=>result (element-type))
        (?amax *blas-state*
               (blas-array-total-size x)
               (cuarray-datum x)
               1
               =>result)
        (cffi:mem-ref =>result (element-type))))))

;; BLAS Level-2
(defmethod gemv (alpha (a cuarray) (x cuarray) beta (y cuarray) &key trans?)
  (check-type alpha number) (check-type beta number)
  (assert-rank a 2) (assert-rank x 1) (assert-rank y 1)
  (with-cublas-function ?gemv
    (with-blas (cublas *blas-state*)
      (cffi:with-foreign-objects ((=>alpha (element-type))
                                  (=>beta (element-type)))
        (setf (cffi:mem-ref =>alpha (element-type))
              (coerce alpha (ecase (element-type)
                              (:float  'single-float)
                              (:double 'double-float))))
        (setf (cffi:mem-ref =>beta (element-type))
              (coerce beta (ecase (element-type)
                              (:float  'single-float)
                              (:double 'double-float))))
        (?gemv *blas-state*
               (if trans? :cublas-op-n :cublas-op-t)
               (blas-array-dimension a 1)
               (blas-array-dimension a 0)
               =>alpha
               (cuarray-datum a) (blas-array-dimension a 1)
               (cuarray-datum x) 1
               =>beta
               (cuarray-datum y) 1)
        y))))

(defmethod ger (alpha (x cuarray) (y cuarray) (a cuarray))
  (check-type alpha number)
  (assert-rank x 1) (assert-rank y 1) (assert-rank a 2)
  (assert-dimension-match x 0 a 0)
  (assert-dimension-match y 0 a 1)
  (with-cublas-function ?ger
    (with-blas (cublas *blas-state*)
      (cffi:with-foreign-object (=>alpha (element-type))
        (setf (cffi:mem-ref =>alpha (element-type))
              (coerce alpha (ecase (element-type)
                              (:float  'single-float)
                              (:double 'double-float))))
        (?ger *blas-state*
              (blas-array-dimension a 1)
              (blas-array-dimension a 0)
              =>alpha
              (cuarray-datum y) 1
              (cuarray-datum x) 1
              (cuarray-datum a) (blas-array-dimension a 1))
        a))))

;;; BLAS Level-3
(defmethod gemm (alpha (a cuarray) (b cuarray) beta (c cuarray) &key trans-a? trans-b?)
  (check-type alpha number) (check-type beta number)
  (assert-rank a 2) (assert-rank b 2) (assert-rank c 2)
  (cond ((and (not trans-a?) (not trans-b?)) (assert-dimension-match c 0 a 0)
                                             (assert-dimension-match c 1 b 1)
                                             (assert-dimension-match a 1 b 0))

        ((and (not trans-a?) trans-b?)       (assert-dimension-match c 0 a 0)
                                             (assert-dimension-match c 1 b 0)
                                             (assert-dimension-match a 1 b 1))

        ((and trans-a? (not trans-b?))       (assert-dimension-match c 0 a 1)
                                             (assert-dimension-match c 1 b 1)
                                             (assert-dimension-match a 0 b 0))

        ((and trans-a? trans-b?)             (assert-dimension-match c 0 a 1)
                                             (assert-dimension-match c 1 b 0)
                                             (assert-dimension-match a 0 b 1)))
  (with-cublas-function ?gemm
    (with-blas (cublas *blas-state*)
      (cffi:with-foreign-objects ((=>alpha (element-type))
                                  (=>beta (element-type)))
        (setf (cffi:mem-ref =>alpha (element-type))
              (coerce alpha (ecase (element-type)
                              (:float  'single-float)
                              (:double 'double-float))))
        (setf (cffi:mem-ref =>beta (element-type))
              (coerce beta (ecase (element-type)
                             (:float  'single-float)
                             (:double 'double-float))))
        (?gemm *blas-state*
               (if trans-a? :cublas-op-t :cublas-op-n)
               (if trans-b? :cublas-op-t :cublas-op-n)
               (blas-array-dimension c 1)
               (blas-array-dimension c 0)
               (blas-array-dimension a (if trans-a? 0 1))
               =>alpha
               (cuarray-datum b) (blas-array-dimension b 1)
               (cuarray-datum a) (blas-array-dimension a 1)
               =>beta
               (cuarray-datum c) (blas-array-dimension c 1))
        c))))

