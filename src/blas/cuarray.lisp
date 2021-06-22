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
    #:amax
    #:amin
    #:asum
    #:axpy
    #:copy
    #:copy*
    #:dot
    #:nrm2
    #:scal

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
  (check-type dimensions list)
  (check-type displaced-to (or null cuarray))
  (mapc (lambda (dim) (check-type dim (integer 0 *))) dimensions)
  (make-instance 'cuarray
                 :dimensions dimensions
                 :datum      (if displaced-to
                                 (cuarray-datum displaced-to)
                                 (clt.cublas:make-device-pointer
                                   *datatype* (reduce #'* dimensions)))))

(defun make-zeros-cuarray (dimensions)
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim (integer 0 *))) dimensions)
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
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim (integer 0 *))) dimensions)
  (check-type init real)
  (check-type step function)
  (check-type key function)
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
(defun array->cuarray (array)
  (check-type array array)
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
  (check-type cuarray cuarray)
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


;;; BLAS functions
(defun amax (x)
  (check-type x cuarray)
  (assert (= (cuarray-rank x) 1) (x)
          'cuarray-rank-error :datum x :expected-rank 1)
  (flet ((%amax (&rest args)
           (ecase *datatype*
             (:float  (apply #'clt.cublas:isamax args))
             (:double (apply #'clt.cublas:idamax args)))))
    (cffi:with-foreign-object (=>result *datatype*)
      (%amax *handle* (cuarray-rank x) (cuarray-datum x) 1 =>result)
      (cffi:mem-ref =>result *datatype*))))

(defun amin (x)
  (check-type x cuarray)
  (assert (= (cuarray-rank x) 1) (x)
          'cuarray-rank-error :datum x :expected-rank 1)
  (flet ((%amin (&rest args)
           (ecase *datatype*
             (:float  (apply #'clt.cublas:isamin args))
             (:double (apply #'clt.cublas:idamin args)))))
    (cffi:with-foreign-object (=>result *datatype*)
      (%amin *handle* (cuarray-rank x) (cuarray-datum x) 1 =>result)
      (cffi:mem-ref =>result *datatype*))))

(defun asum (x)
  (check-type x cuarray)
  (assert (= (cuarray-rank x) 1) (x)
          'cuarray-rank-error :datum x :expected-rank 1)
  (flet ((%asum (&rest args)
           (ecase *datatype*
             (:float  (apply #'clt.cublas:sasum args))
             (:double (apply #'clt.cublas:dasum args)))))
    (cffi:with-foreign-object (=>result *datatype*)
      (%asum *handle* (cuarray-dimension x 0) (cuarray-datum x) 1 =>result)
      (cffi:mem-ref =>result *datatype*))))

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
           (ecase *datatype*
             (:float  (apply #'clt.cublas:saxpy args))
             (:double (apply #'clt.cublas:daxpy args)))))
    (cffi:with-foreign-object (=>alpha *datatype*)
      (setf (cffi:mem-ref =>alpha *datatype*)
        (coerce alpha (ecase *datatype*
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
           (ecase *datatype*
             (:float  (apply #'clt.cublas:scopy args))
             (:double (apply #'clt.cublas:dcopy args)))))
    (%copy *handle* (cuarray-total-size x) (cuarray-datum x) 1 (cuarray-datum y) 1)))

(defun copy* (x y &key count (stride-x 1) (stride-y 1))
  (check-type x cuarray)
  (check-type y cuarray)
  (check-type count (or null (integer 0 *)))
  (if (null count)
      (assert (<= (/ (cuarray-total-size x) stride-x) (/ (cuarray-total-size y) stride-y)) (x y stride-x stride-y)
              'simple-cuarray-error
              :format-control "The number of elements in~%~2T~S~%at stride~%~2T~D~% is greater than in~%~2T~S~%at stride~%~2T~D~%."
              :format-args (list x stride-x y stride-y))
      (progn
        (assert (<= count (/ (cuarray-total-size x) stride-x)) (x count stride-x)
                'simple-cuarray-error
                :format-control "The count~%~2T~D~%is greater than the number of elements in~%~2T~S~%at stride~%~2T~D~%."
                :format-args (list count x stride-x))
        (assert (<= count (/ (cuarray-total-size y) stride-y)) (y count stride-y)
                'simple-cuarray-error
                :format-control "The count~%~2T~D~%is greater than the number of elements in~%~2T~S~%at stride~%~2T~D~%."
                :format-args (list count y stride-y))))
  (flet ((%copy (&rest args)
           (ecase *datatype*
             (:float  (apply #'clt.cublas:scopy args))
             (:double (apply #'clt.cublas:dcopy args)))))
    (let ((count (or count (cuarray-total-size x))))
      (%copy *handle* count (cuarray-datum x) stride-x (cuarray-datum y) stride-y))))

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
          (ecase *datatype*
            (:float  (apply #'clt.cublas:sdot args))
            (:double (apply #'clt.cublas:ddot args)))))
   (cffi:with-foreign-object (=>result *datatype*)
     (%dot *handle* (cuarray-dimension x 0) (cuarray-datum x) 1 (cuarray-datum y) 1 =>result)
     (cffi:mem-ref =>result *datatype*))))

(defun nrm2 (x)
  (check-type x cuarray)
  (assert (= (cuarray-rank x) 1) (x)
          'cuarray-rank-error :datum x :expected-rank 1)
  (flet ((%nrm2 (&rest args)
           (ecase *datatype*
             (:float  (apply #'clt.cublas:snrm2 args))
             (:double (apply #'clt.cublas:dnrm2 args)))))
    (cffi:with-foreign-object (=>result *datatype*)
      (%nrm2 *handle* (cuarray-dimension x 0) (cuarray-datum x) 1 =>result)
      (cffi:mem-ref =>result *datatype*))))

(defun scal (alpha x)
  (check-type alpha real)
  (check-type x cuarray)
  (assert (= (cuarray-rank x) 1) (x)
          'cuarray-rank-error :datum x :expected-rank 1)
  (flet ((%scal (&rest args)
           (ecase *datatype*
             (:float  (apply #'clt.cublas:sscal args))
             (:double (apply #'clt.cublas:dscal args)))))
    (cffi:with-foreign-object (=>alpha *datatype*)
      (setf (cffi:mem-ref =>alpha *datatype*) (coerce alpha (ecase *datatype*
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
           (ecase *datatype*
             (:float  (apply #'clt.cublas:sgemv args))
             (:double (apply #'clt.cublas:dgemv args)))))
    (let ((m (cuarray-dimension a 0))
          (n (cuarray-dimension a 1))
          (trans (if trans-a? :cublas-op-t :cublas-op-n)))
      (cffi:with-foreign-objects ((=>alpha *datatype*)
                                  (=>beta *datatype*))
        (setf (cffi:mem-ref =>alpha *datatype*)
              (coerce alpha (ecase *datatype*
                              (:float  'single-float)
                              (:double 'double-float))))
        (setf (cffi:mem-ref =>beta *datatype*)
              (coerce beta (ecase *datatype*
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
           (ecase *datatype*
             (:float  (apply #'clt.cublas:sger args))
             (:double (apply #'clt.cublas:dger args)))))
    (let ((m (cuarray-dimension a 0))
          (n (cuarray-dimension a 1)))
      (cffi:with-foreign-object (=>alpha *datatype*)
        (setf (cffi:mem-ref =>alpha *datatype*)
              (coerce alpha (ecase *datatype*
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
           (ecase *datatype*
             (:float  (apply #'clt.cublas:sgemm args))
             (:double (apply #'clt.cublas:dgemm args)))))
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

(defun geam (alpha a b beta c &key trans-a? trans-b?)
  (check-type alpha real)
  (check-type a cuarray)
  (check-type b cuarray)
  (check-type c cuarray)
  (assert (= (cuarray-rank a) 2) (a)
          'cuarray-rank-error :datum a :expected-rank 2)
  (assert (= (cuarray-rank b) 2) (b)
          'cuarray-rank-error :datum b :expected-rank 2)
  (assert (= (cuarray-rank c) 2) (c)
          'cuarray-rank-error :datum c :expected-rank 2)
  (cond
    ((and (not trans-a?) (not trans-b?))
     (assert (= (cuarray-dimension a 0) (cuarray-dimension b 0)) (a b)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 0 :datum2 b :axis2 0)
     (assert (= (cuarray-dimension a 1) (cuarray-dimension b 1)) (a b)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 1 :datum2 b :axis2 1)
     (assert (= (cuarray-dimension b 0) (cuarray-dimension c 0)) (b c)
             'cuarray-dimension-unmatched-error :datum1 b :axis1 0 :datum2 c :axis2 0)
     (assert (= (cuarray-dimension b 1) (cuarray-dimension c 1)) (b c)
             'cuarray-dimension-unmatched-error :datum1 b :axis1 1 :datum2 c :axis2 1))
    ((and trans-a? (not trans-b?))
     (assert (= (cuarray-dimension a 1) (cuarray-dimension b 0)) (a b)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 1 :datum2 b :axis2 0)
     (assert (= (cuarray-dimension a 0) (cuarray-dimension b 1)) (a b)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 0 :datum2 b :axis2 1)
     (assert (= (cuarray-dimension b 0) (cuarray-dimension c 0)) (b c)
             'cuarray-dimension-unmatched-error :datum1 b :axis1 0 :datum2 c :axis2 0)
     (assert (= (cuarray-dimension b 1) (cuarray-dimension c 1)) (b c)
             'cuarray-dimension-unmatched-error :datum1 b :axis1 1 :datum2 c :axis2 1))
    ((and (not trans-a?) trans-b?)
     (assert (= (cuarray-dimension a 0) (cuarray-dimension b 1)) (a b)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 0 :datum2 b :axis2 1)
     (assert (= (cuarray-dimension a 1) (cuarray-dimension b 0)) (a b)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 1 :datum2 b :axis2 0)
     (assert (= (cuarray-dimension b 1) (cuarray-dimension c 0)) (b c)
             'cuarray-dimension-unmatched-error :datum1 b :axis1 1 :datum2 c :axis2 0)
     (assert (= (cuarray-dimension b 0) (cuarray-dimension c 1)) (b c)
             'cuarray-dimension-unmatched-error :datum1 b :axis1 0 :datum2 c :axis2 1))
    (t
     (assert (= (cuarray-dimension a 0) (cuarray-dimension b 0)) (a b)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 0 :datum2 b :axis2 0)
     (assert (= (cuarray-dimension a 1) (cuarray-dimension b 1)) (a b)
             'cuarray-dimension-unmatched-error :datum1 a :axis1 1 :datum2 b :axis2 1)
     (assert (= (cuarray-dimension b 0) (cuarray-dimension c 0)) (b c)
             'cuarray-dimension-unmatched-error :datum1 b :axis1 0 :datum2 c :axis2 0)
     (assert (= (cuarray-dimension b 1) (cuarray-dimension c 1)) (b c)
             'cuarray-dimension-unmatched-error :datum1 b :axis1 1 :datum2 c :axis2 1)))
  (flet ((%geam (&rest args)
           (ecase *datatype*
             (:float  (apply #'clt.cublas:sgeam args))
             (:double (apply #'clt.cublas:dgeam args)))))
    (cffi:with-foreign-objects ((=>alpha *datatype*)
                                 (=>beta *datatype*))
      (setf (cffi:mem-ref =>alpha *datatype*)
        (coerce alpha (ecase *datatype*
                        (:float  'single-float)
                        (:double 'double-float))))
      (setf (cffi:mem-ref =>beta *datatype*)
        (coerce beta (ecase *datatype*
                       (:float  'single-float)
                       (:double 'double-float))))
      (%geam
        *handle*
        (if trans-a? :cublas-op-t :cublas-op-n)
        (if trans-b? :cublas-op-t :cublas-op-n)
        (cuarray-dimension c 0)
        (cuarray-dimension c 1)
        =>alpha
        (cuarray-datum a) (cuarray-dimension a 0)
        =>beta
        (cuarray-datum b) (cuarray-dimension b 0)
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
