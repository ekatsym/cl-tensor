(defpackage cl-tensor.blas.cuarray
  (:nicknames :clt.cuarray clt.b.ca)
  (:use :common-lisp :cl-tensor.blas :cl-tensor.util :cl-tensor.blas.assert)
  (:import-from cl-tensor.blas.cublas #:create-handle #:destroy)
  (:export
    #:cuarray->array))
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
         (cuarr (make-cuarray (array-dimensions array)))
         (datum (cuarray-datum cuarr))
         (count (reduce #'* dims)))
    (cffi:with-foreign-object (carr (element-type) count)
      (dotimes (i (reduce #'* dims))
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
      (dotimes (i (reduce #'* dims))
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
    (copy* x y :stride-y (1+ n))))

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
        y))) )



#|
  ;;; Core
  (defparameter *handle* nil)
(defparameter (element-type) :float)

(defmacro with-handle ((&optional handle) &body body)
  (if handle
      `(let ((*handle* ,handle)) (progn ,@body))
      `(clt.cublas:with-handle *handle* ,@body)))


;;; Constructors


(defun make-zeros-cuarray (dimensions)
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim (integer 0 *))) dimensions)
  (let* ((cuarr (make-cuarray dimensions))
         (total-size (cuarray-total-size cuarr)))
    (cffi:with-foreign-object (=>x (element-type) total-size)
      (dotimes (i total-size)
        (setf (cffi:mem-aref =>x (element-type) i)
              (ecase (element-type)
                (:float 0.0f1)
                (:double 0.0d1))))
      (clt.cublas:cuda-memcpy (cuarray-datum cuarr)
                              =>x
                              (* (cffi:foreign-type-size (element-type)) total-size)
                              :cuda-memcpy-default)
      cuarr)))



(defun identity-cuarray (rank)
  (make-custom-cuarray (list rank rank)
                       :key (lambda (n) (if (multiple-value-call #'= (floor n rank))
                                            1.0
                                            0.0))))


;;; Accessors
(defun cuarray-rank (cuarray)
  (check-type cuarray cuarray)
  (length (cuarray-dimensions cuarray)))

(defun cuarray-dimension (cuarray axis-number)
  (check-type cuarray cuarray)
  (check-type axis-number (integer 0 *))
  (nth axis-number (cuarray-dimensions cuarray)))

(defun cuarray-total-size (cuarray)
  (check-type cuarray cuarray)
  (reduce #'* (cuarray-dimensions cuarray) :initial-value 1))


;;; Convertors



;;; BLAS functions
(defun amax (x)
  (check-type x cuarray)
  (assert (= (cuarray-rank x) 1) (x)
          'cuarray-rank-error :datum x :expected-rank 1)
  (flet ((%amax (&rest args)
           (ecase (element-type)
             (:float  (apply #'clt.cublas:isamax args))
             (:double (apply #'clt.cublas:idamax args)))))
    (cffi:with-foreign-object (=>result (element-type))
      (%amax *handle* (cuarray-rank x) (cuarray-datum x) 1 =>result)
      (cffi:mem-ref =>result (element-type)))))

(defun amin (x)
  (check-type x cuarray)
  (assert (= (cuarray-rank x) 1) (x)
          'cuarray-rank-error :datum x :expected-rank 1)
  (flet ((%amin (&rest args)
           (ecase (element-type)
             (:float  (apply #'clt.cublas:isamin args))
             (:double (apply #'clt.cublas:idamin args)))))
    (cffi:with-foreign-object (=>result (element-type))
      (%amin *handle* (cuarray-rank x) (cuarray-datum x) 1 =>result)
      (cffi:mem-ref =>result (element-type)))))

(defun asum (x)
  (check-type x cuarray)
  (assert (= (cuarray-rank x) 1) (x)
          'cuarray-rank-error :datum x :expected-rank 1)
  (flet ((%asum (&rest args)
           (ecase (element-type)
             (:float  (apply #'clt.cublas:sasum args))
             (:double (apply #'clt.cublas:dasum args)))))
    (cffi:with-foreign-object (=>result (element-type))
      (%asum *handle* (cuarray-dimension x 0) (cuarray-datum x) 1 =>result)
      (cffi:mem-ref =>result (element-type)))))

(defun axpy (alpha x y)
  (check-type x cuarray)
  (check-type y cuarray)
  (assert (= (cuarray-rank x) 1) (x)
          'cuarray-rank-error :datum x :expected-rank 1)
  (assert (= (cuarray-rank y) 1) (y)
          'cuarray-rank-error :datum y :expected-rank 1)
  (assert (= (cuarray-dimension x 0) (cuarray-dimension y 0)) (x y)
          'cuarray-dimension-unmatched-error :datam1 x :axis1 0 :datam2 y :axis2)
  (flet ((%axpy (&rest args)
           (ecase (element-type)
             (:float  (apply #'clt.cublas:saxpy args))
             (:double (apply #'clt.cublas:daxpy args)))))
    (cffi:with-foreign-object (=>alpha (element-type))
      (setf (cffi:mem-ref =>alpha (element-type))
        (coerce alpha (ecase (element-type)
                        (:float  'single-float)
                        (:double 'double-float))))
      (%axpy *handle* (cuarray-dimension x 0) =>alpha (cuarray-datum x) 1 (cuarray-datum y) 1)
      y)))

(defun copy (x y)
  (check-type x cuarray)
  (check-type y cuarray)
  (assert (<= (cuarray-total-size x) (cuarray-total-size y)) (x y)
          'simple-cuarray-error
          :format-control "The total size of ~%~2T~S~%is greater than~%~2T~S~%."
          :format-args x y)
  (flet ((%copy (&rest args)
           (ecase (element-type)
             (:float  (apply #'clt.cublas:scopy args))
             (:double (apply #'clt.cublas:dcopy args)))))
    (%copy *handle* (cuarray-total-size x) (cuarray-datum x) 1 (cuarray-datum y) 1)))


(defun dot (x y)
 (check-type x cuarray)
 (check-type y cuarray)
 (assert (= (cuarray-rank x) 1) (x)
         'cuarray-rank-error :datum x :expected-rank 1)
 (assert (= (cuarray-rank y) 1) (y)
         'cuarray-rank-error :datum y :expected-rank 1)
 (assert (= (cuarray-dimension x 0) (cuarray-dimension y 0)) (x y)
         'cuarray-dimension-unmatched-error :datum1 x :axis1 0 :datum2 y :axis2 0)
 (flet ((%dot (&rest args)
          (ecase (element-type)
            (:float  (apply #'clt.cublas:sdot args))
            (:double (apply #'clt.cublas:ddot args)))))
   (cffi:with-foreign-object (=>result (element-type))
     (%dot *handle* (cuarray-dimension x 0) (cuarray-datum x) 1 (cuarray-datum y) 1 =>result)
     (cffi:mem-ref =>result (element-type)))))

(defun nrm2 (x)
  (check-type x cuarray)
  (assert (= (cuarray-rank x) 1) (x)
          'cuarray-rank-error :datum x :expected-rank 1)
  (flet ((%nrm2 (&rest args)
           (ecase (element-type)
             (:float  (apply #'clt.cublas:snrm2 args))
             (:double (apply #'clt.cublas:dnrm2 args)))))
    (cffi:with-foreign-object (=>result (element-type))
      (%nrm2 *handle* (cuarray-dimension x 0) (cuarray-datum x) 1 =>result)
      (cffi:mem-ref =>result (element-type)))))

(defun scal (alpha x)
  (check-type alpha real)
  (check-type x cuarray)
  (assert (= (cuarray-rank x) 1) (x)
          'cuarray-rank-error :datum x :expected-rank 1)
  (flet ((%scal (&rest args)
           (ecase (element-type)
             (:float  (apply #'clt.cublas:sscal args))
             (:double (apply #'clt.cublas:dscal args)))))
    (cffi:with-foreign-object (=>alpha (element-type))
      (setf (cffi:mem-ref =>alpha (element-type)) (coerce alpha (ecase (element-type)
                                                              (:float  'single-float)
                                                              (:double 'double-float))))
      (%scal *handle* (cuarray-dimension x 0) =>alpha (cuarray-datum x) 1)
      x)))

(defun gemv (alpha a x beta y &key trans-a?)
  (declare (optimize (debug 3)))
  (check-type alpha real)
  (check-type a cuarray)
  (check-type x cuarray)
  (check-type y cuarray)
  (assert (= (cuarray-rank a) 2) (a)
          'cuarray-rank-error :datum a :expected-rank 2)
  (assert (= (cuarray-rank x) 1) (x)
          'cuarray-rank-error :datum x :expected-rank 1)
  (assert (= (cuarray-rank y) 1) (y)
          'cuarray-rank-error :datum y :expected-rank 1)
  (if trans-a?
      (progn
        (assert (= (cuarray-dimension a 0) (cuarray-dimension x 0)) (a x)
                'cuarray-dimension-unmatched-error :datum1 a :axis1 0 :datum2 x :axis2 0)
        (assert (= (cuarray-dimension a 1) (cuarray-dimension y 0)) (x y)
                'cuarray-dimension-unmatched-error :datum1 x :axis1 1 :datum2 y :axis2 0))
      (progn
        (assert (= (cuarray-dimension a 1) (cuarray-dimension x 0)) (a x)
                'cuarray-dimension-unmatched-error :datum1 a :axis1 1 :datum2 x :axis2 0)
        (assert (= (cuarray-dimension a 0) (cuarray-dimension y 0)) (x y)
                'cuarray-dimension-unmatched-error :datum1 x :axis1 0 :datum2 y :axis2 0)))
  (flet ((%gemv (&rest args)
           (ecase (element-type)
             (:float  (apply #'clt.cublas:sgemv args))
             (:double (apply #'clt.cublas:dgemv args)))))
    (let ((m (cuarray-dimension a 0))
          (n (cuarray-dimension a 1))
          (trans (if trans-a? :cublas-op-t :cublas-op-n)))
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
        (%gemv *handle* trans m n =>alpha (cuarray-datum a) m (cuarray-datum x) 1 =>beta (cuarray-datum y) 1)
        y))))

(defun ger (alpha x y a)
  (check-type alpha real)
  (check-type x cuarray)
  (check-type y cuarray)
  (check-type a cuarray)
  (assert (= (cuarray-rank x) 1) (x)
          'cuarray-rank-error :datum x :expected-rank 1)
  (assert (= (cuarray-rank y) 1) (y)
          'cuarray-rank-error :datum y :expected-rank 1)
  (assert (= (cuarray-rank a) 2) (a)
          'cuarray-rank-error :datum a :expected-rank 2)
  (assert (= (cuarray-dimension x 0) (cuarray-dimension a 0)) (x a)
          'cuarray-dimension-unmatched-error :datum1 x :axis1 0 :datum2 a :axis2 0)
  (assert (= (cuarray-dimension y 0) (cuarray-dimension a 1)) (y a)
          'cuarray-dimension-unmatched-error :datum1 y :axis1 0 :datum2 a :axis2 1)
  (flet ((%ger (&rest args)
           (ecase (element-type)
             (:float  (apply #'clt.cublas:sger args))
             (:double (apply #'clt.cublas:dger args)))))
    (let ((m (cuarray-dimension a 0))
          (n (cuarray-dimension a 1)))
      (cffi:with-foreign-object (=>alpha (element-type))
        (setf (cffi:mem-ref =>alpha (element-type))
              (coerce alpha (ecase (element-type)
                              (:float  'single-float)
                              (:double 'double-float))))
        (%ger *handle* m n =>alpha (cuarray-datum x) 1 (cuarray-datum y) 1 (cuarray-datum a) m)
        a))))

#|
(let ((x (make-custom-cuarray '(5)))
      (y (make-custom-cuarray '(5) :init 1 :step #'identity))
      (a (make-cuarray '(5 5))))
  (with-handle ()
    (print x)
    (print y)
    (ger 1.0 x y a)))
|#

(defun gemm (alpha a b beta c &key trans-a? trans-b?)
  (check-type alpha real)
  (check-type a cuarray)
  (check-type b cuarray)
  (check-type beta real)
  (check-type c cuarray)
  (assert (= (cuarray-rank a) 2) (a)
          'cuarray-rank-error :datum a :expected-rank 2)
  (assert (= (cuarray-rank b) 2) (b)
          'cuarray-rank-error :datum b :expected-rank 2)
  (assert (= (cuarray-rank c) 2) (c)
          'cuarray-rank-error :datum c :expected-rank 2)
  (cond
    ((and (not trans-a?) (not trans-b?))
     (assert (= (cuarray-dimension a 1) (cuarray-dimension b 0)) (a b)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 1 :datum2 b :axis2 0)
     (assert (= (cuarray-dimension a 0) (cuarray-dimension c 0)) (a c)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 0 :datum2 c :axis2 0)
     (assert (= (cuarray-dimension b 1) (cuarray-dimension c 1)) (b c)
             'cuarray-dimension-unmatched-error :datum1 b :axis1 1 :datum2 c :axis2 1))
    ((and trans-a? (not trans-b?))
     (assert (= (cuarray-dimension a 0) (cuarray-dimension b 0)) (a b)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 0 :datum2 b :axis2 0)
     (assert (= (cuarray-dimension a 1) (cuarray-dimension c 0)) (a c)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 1 :datum2 c :axis2 0)
     (assert (= (cuarray-dimension b 1) (cuarray-dimension c 1)) (b c)
             'cuarray-dimension-unmatched-error :datum1 b :axis1 1 :datum2 c :axis2 1))
    ((and (not trans-a?) trans-b?)
     (assert (= (cuarray-dimension a 1) (cuarray-dimension b 1)) (a b)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 1 :datum2 b :axis2 1)
     (assert (= (cuarray-dimension a 0) (cuarray-dimension c 0)) (a c)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 0 :datum2 c :axis2 0)
     (assert (= (cuarray-dimension b 0) (cuarray-dimension c 1)) (b c)
             'cuarray-dimension-unmatched-error :datum1 b :axis1 0 :datum2 c :axis2 1))
    (t
     (assert (= (cuarray-dimension a 0) (cuarray-dimension b 1)) (a b)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 0 :datum2 b :axis2 1)
     (assert (= (cuarray-dimension a 1) (cuarray-dimension c 0)) (a c)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 1 :datum2 c :axis2 0)
     (assert (= (cuarray-dimension b 0) (cuarray-dimension c 1)) (b c)
             'cuarray-dimension-unmatched-error :datum1 b :axis1 0 :datum2 c :axis2 1)))
  (flet ((%gemm (&rest args)
           (ecase (element-type)
             (:float  (apply #'clt.cublas:sgemm args))
             (:double (apply #'clt.cublas:dgemm args)))))
    (cffi:with-foreign-objects ((=>alpha (element-type))
                                 (=>beta  (element-type)))
      (setf (cffi:mem-ref =>alpha (element-type))
        (coerce alpha
                (ecase (element-type)
                  (:float 'single-float)
                  (:double 'double-float))))
      (setf (cffi:mem-ref =>beta  (element-type))
        (coerce beta
                (ecase (element-type)
                  (:float 'single-float)
                  (:double 'double-float))))
      (%gemm
        *handle*
        (if trans-a? :cublas-op-t :cublas-op-n)
        (if trans-b? :cublas-op-t :cublas-op-n)
        (cuarray-dimension c 0)
        (cuarray-dimension c 1)
        (if trans-a? (cuarray-dimension a 0) (cuarray-dimension a 1))
        =>alpha
        (cuarray-datum a) (cuarray-dimension a 0)
        (cuarray-datum b) (cuarray-dimension b 0)
        =>beta
        (cuarray-datum c) (cuarray-dimension c 0))
      c)))



(define-condition cuarray-cuarray-error (simple-error) ())

(define-condition cuarray-rank-error (error)
  ((expected-rank :initarg :expected-rank
                  :reader cuarray-rank-error-expected-rank)
   (datum :initarg :datum
          :reader cuarray-rank-error-datum))
  (:report (lambda (o s)
             (format s "The rank of the value~%~2T~S~%is not ~%~2T~S"
                     (cuarray-rank-error-datum o)
                     (cuarray-rank-error-expected-rank o)))))

(define-condition cuarray-dimension-unmatched-error (error)
  ((datum1 :initarg :datum1
           :reader cuarray-dimension-unmatched-error-datum1)
   (axis1 :initarg :axis1
          :reader cuarray-dimension-unmatched-error-axis1)
   (datum2 :initarg :datum2
           :reader cuarray-dimension-unmatched-error-datum2)
   (axis2 :initarg :axis2
          :reader cuarray-dimension-unmatched-error-axis2))
  (:report (lambda (o s)
             (format s
                     "The dimensions of the axis~%~2T~D~%of the CUARRAY~%~2T~S~%and axis~%~2T~D~%of the CUARRAY~%~2T~S~%are different."
                     (cuarray-dimension-unmatched-error-axis1 o)
                     (cuarray-dimension-unmatched-error-datum1 o)
                     (cuarray-dimension-unmatched-error-axis2 o)
                     (cuarray-dimension-unmatched-error-datum2 o)))))
|#
