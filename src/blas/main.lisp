(defpackage cl-tensor.blas
  (:nicknames :clt.blas)
  (:use :common-lisp)
  (:export
    ;; Core
    #:array #:open-blas #:cublas

    ;; State
    #:*blas-state* #:start-blas #:finish-blas #:with-blas

    ;; Element Type
    #:element-type #:*element-type* #:s #:d #:c #:z

    ;; Constructor
    #:make-blas-array #:make-blas-array*
    #:make-identity-matrix

    ;; Accessor
    #:blas-array-dimensions
    #:blas-array-dimension
    #:blas-array-rank
    #:blas-array-total-size

    ;; Utility
    #:trans #:diag #:reshape

    ;; BLAS Level-1
    #:scal #:copy #:axpy #:dot
    #:nrm2 #:asum #:amax

    ;; BLAS Level-2
    #:gemv #:gbmv #:ger

    ;; BLAS Level-3
    #:gemm))
(in-package :cl-tensor.blas)


;;; State
(defparameter *blas-state* nil)

(defgeneric start-blas (blas-name))

(defgeneric finish-blas (blas-name state))

(defmacro with-blas ((blas-name &optional state) &body body)
  `(if ,state
       (let ((*blas-state* ,state))
         ,@body)
       (let ((*blas-state* (start-blas ',blas-name)))
         (unwind-protect (multiple-value-prog1 (progn ,@body))
           (finish-blas ',blas-name *blas-state*)))))

;;; Element Type
(deftype element-type ()
  '(member s d c z))

(defparameter *element-type* 's)


;;; Constructor
(defgeneric make-blas-array (blas-name dimensions))

(defgeneric make-blas-array* (blas-name dimensions &key init step key))

(defun make-identity-matrix (blas-name rank)
  (make-blas-array* blas-name
                    (list rank rank)
                    :key (lambda (n) (if (multiple-value-call #'= (floor n rank))
                                         1.0
                                         0.0))))


;;; Accessor
(defgeneric blas-array-dimensions (blas-array))

(defun blas-array-dimension (blas-array axis-number)
  (nth axis-number (blas-array-dimensions blas-array)))

(defun blas-array-rank (blas-array)
  (length (blas-array-dimensions blas-array)))

(defun blas-array-total-size (blas-array)
  (reduce #'* (blas-array-dimensions blas-array) :initial-value 1))


;;; Converter
(defgeneric coerce-blas-array (blas-array output-blas-name))


;;; Utility
(defgeneric trans (a)
  (:documentation
"Return the transposition of A.
A: matrix."))

(defgeneric diag (x)
  (:documentation
"Return diagnal matrix of X.
X: vector."))

(defgeneric reshape (dimensions a)
  (:documentation
"Return DIMENSIONS blas-array that has elements of A.
A: blas-array."))


;;; BLAS

;; BLAS Level-1
(defgeneric scal (alpha x)
  (:documentation
"X <- ALPHA * X.
Return X.
ALPHA: scalar.
X: vector."))

(defgeneric copy (x y &key count stride-x stride-y)
  (:documentation
"Y <- X.
Return Y.
X: vector.
Y: vector."))

(defgeneric axpy (alpha x y)
  (:documentation
"Y <- ALPHA * X.
ALPHA: scalar.
X: vector.
Y: vector."))

(defgeneric dot (x y)
  (:documentation
"Return X^T * Y.
X: vector.
Y: vector."))

(defgeneric nrm2 (x)
  (:documentation
"Return X^T * X.
X: vector."))

(defgeneric asum (x)
  (:documentation
"Return sum {|x_i| \in X}."))

(defgeneric amax (x)
  (:documentation
    "Return argmax_{x_i \in X} |x_i|."))

;; BLAS Level-2
(defgeneric gemv (alpha a x beta y &key trans? diag?)
  (:documentation
"Y <- ALPHA * A * X + BETA * Y.
Y <- ALPHA * A^T * X + BETA * Y.
Y <- ALPHA * A^H * X + BETA * Y.
Return Y.
ALPHA: scalar.
A: matrix.
X: vector.
BETA: scalar.
Y: vector."))

(defgeneric gbmv (alpha a x beta y &key trans? diag? band-width)
  (:documentation
"Y <- ALPHA * A * X + BETA * Y.
Y <- ALPHA * A^T * X + BETA * Y.
Y <- ALPHA * A^H * X + BETA * Y.
Return Y.
ALPHA: scalar.
A: band matrix with BAND-WIDTH.
X: vector.
BETA: scalar.
Y: vector."))

(defgeneric ger (alpha x y a)
  (:documentation
"A <- ALPHA * X * Y^T.
Return A.
ALPHA: scalar.
X: vector.
Y: vector.
A: matrix."))

;;; BLAS Level-3
(defgeneric gemm (alpha a b beta c &key trans-a? trans-b?)
  (:documentation
"C <- ALPHA * op(A) * op(B) + BETA * C.
Return C.
ALPHA: scalar.
A: matrix.
B: matrix.
BETA: scalar.
C: matrix.
op(X) = X, X^T, X^H."))
