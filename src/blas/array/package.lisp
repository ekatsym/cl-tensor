(defpackage cl-tensor.blas.array
  (:nicknames :clt.blas.array :clt.b.ar)
  (:use
    :common-lisp
    :cl-tensor.core :cl-tensor.util
    :cl-tensor.blas :cl-tensor.blas.util))
(in-package :cl-tensor.blas.array)


;;; Helper
(defun map-array (f a)
  (let* ((dims (array-dimensions a))
         (b (make-array dims))
         (n (array-total-size a)))
    (dotimes (i n)
      (setf (row-major-aref b i)
            (funcall f (row-major-aref a i))))
    b))

(defun map-array! (f a)
  (let ((n (array-total-size a)))
    (dotimes (i n)
      (setf (row-major-aref a i)
            (funcall f (row-major-aref a i))))
    a))

;;; Constructor
(defmethod make-blas-array ((array-type-spec (eql 'array)) dimensions
                            &key (element-type 'single-float)
                                 (initial-element 0.0)
                                 displaced-to)
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim index)) dimensions)
  (assert (subtypep element-type 'element-type) (element-type))
  (setq initial-element (coerce initial-element element-type))
  (make-array dimensions :element-type element-type
                         :initial-element initial-element
                         :displaced-to displaced-to))

(defmethod make-blas-array* ((array-type-spec (eql 'array)) dimensions
                             &key (element-type 'single-float)
                                  (seed 0.0) (next #'1+) (key #'identity))
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim index)) dimensions)
  (assert (subtypep element-type 'element-type) (element-type))
  (check-type next function)
  (check-type key function)
  (let ((a (make-array dimensions)))
    (dotimes (i (array-total-size a))
      (let ((x (coerce (funcall key seed) element-type)))
        (setq seed (funcall next seed))
        (setf (row-major-aref a i) x)))
    a))

;;; Accessors
(defmethod blas-array-dimensions ((blas-array array))
  (array-dimensions blas-array))

;;; Utilities
(defmethod trans ((a array))
  (assert-rank a 2)
  (let* ((dims (array-dimensions a))
         (m (first dims))
         (n (second dims))
         (b (make-array (list n m))))
    (dotimes (i m)
      (dotimes (j n)
        (setf (aref b j i) (aref a i j))))
    b))

(defmethod diag ((x array))
  (assert-rank x 1)
  (let* ((n (array-dimension x 0))
         (a (make-array (list n n))))
    (dotimes (i n)
      (setf (aref a i i) (aref x i)))
    a))

(defmethod reshape (dimensions (a array))
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim index)) dimensions)
  (assert-total-size a (reduce #'* dimensions))
  (make-array dimensions :displaced-to a))

;;; BLAS Level-1
(defmethod scal (alpha (x array))
  (dotimes (i (array-total-size x))
    (setf (row-major-aref x i) (* alpha (row-major-aref x i))))
  x)

(defmethod copy ((x array) (y array) &key count (stride-x 1) (stride-y 1))
  (let ((nx (array-total-size x))
        (ny (array-total-size y)))
    (do ((xi 0 (+ xi stride-x))
         (yi 0 (+ yi stride-y))
         (c (or count (array-total-size x)) (1- c)))
        ((or (zerop c) (>= xi nx) (>= yi ny)) y)
        (setf (row-major-aref y yi) (row-major-aref x xi)))))

(defmethod axpy (alpha (x array) (y array))
  (assert-total-size-match x y)
  (dotimes (i (array-total-size x))
    (incf (row-major-aref y i) (* alpha (row-major-aref x i))))
  y)

(defmethod dot ((x array) (y array))
  (assert-total-size-match x y)
  (loop for i below (array-total-size x)
        sum (* (row-major-aref x i) (row-major-aref y i))))

(defmethod nrm2 ((x array))
  (dot x x))

(defmethod asum ((x array))
  (loop for i below (array-total-size x)
        sum (abs (row-major-aref x i))))

(defmethod amax ((x array))
  (let* ((arg (row-major-aref x 0))
         (val (abs arg)))
    (dotimes (i (array-total-size x))
      (let* ((new-arg (row-major-aref x i))
             (new-val (abs new-arg)))
        (when (> new-val val)
          (setq arg new-arg)
          (setq val new-val))))
    arg))

(defmethod amin ((x array))
  (let* ((arg (row-major-aref x 0))
         (val (abs arg)))
    (dotimes (i (array-total-size x))
      (let* ((new-arg (row-major-aref x i))
             (new-val (abs new-arg)))
        (when (< new-val val)
          (setq arg new-arg)
          (setq val new-val))))
    arg))
