(defpackage cl-tensor.blas.util
  (:nicknames :clt.blas.util :clt.b.u)
  (:use :common-lisp :cl-tensor.blas)
  (:export
    #:assert-dimensions
    #:assert-dimensions-match
    #:assert-rank
    #:assert-rank-match
    #:assert-dimension
    #:assert-dimension-match
    #:assert-total-size
    #:assert-total-size-match))
(in-package :cl-tensor.blas.util)


;;; Definition of Conditions
(defmacro define-blas-condition (name &body format-controls)
  (let ((g!o (gensym))
        (g!s (gensym))
        (format-control (reduce (lambda (fc acc) (concatenate 'string fc acc))
                                format-controls
                                :from-end t)))
    `(define-condition ,name (type-error)
       ()
       (:report (lambda (,g!o ,g!s)
                  (format ,g!s ,format-control
                          (type-error-datum ,g!o)
                          (type-error-expected-type ,g!o)))))))

(define-blas-condition dimensions-error
  "The blas-array~%"
  "  ~S~%"
  "is not of dimensions~%"
  "  ~S~%")

(define-blas-condition rank-error
  "The blas-array~%"
  "  ~S~%"
  "is not of rank~%"
  "  ~S~%")

(define-blas-condition dimension-error
  "The blas-array~%"
  "  ~S~%"
  "is not of dimension of axis-number"
  " ~{~S~^~%  ~S~%~}~%")

(define-blas-condition total-size-error
  "The blas-array~%"
  "  ~S~%"
  "is not of total-size~%"
  "  ~S~%")

(define-condition blas-unmatch-error (error)
  ((datum1 :initarg :datum1 :reader blas-unmatch-error-datum1)
   (datum2 :initarg :datum2 :reader blas-unmatch-error-datum2)))

(defmacro define-blas-unmatch-condition (name &body format-controls)
  (let ((g!o (gensym))
        (g!s (gensym))
        (format-control (reduce (lambda (fc acc) (concatenate 'string fc acc))
                                format-controls
                                :from-end t)))
    `(define-condition ,name (blas-unmatch-error)
       ()
       (:report (lambda (,g!o ,g!s)
                  (format ,g!s ,format-control
                          (blas-unmatch-error-datum1 ,g!o)
                          (blas-unmatch-error-datum2 ,g!o)))))))

(define-blas-unmatch-condition dimensions-unmatch-error
  "The dimensionss of~%"
  "  ~S~%"
  "and~%"
  "  ~S~%"
  "unmatched.~%")

(define-blas-unmatch-condition rank-unmatch-error
  "The ranks of~%"
  "  ~S~%"
  "and~%"
  "  ~S~%"
  "unmatched.~%")

(define-blas-unmatch-condition dimension-unmatch-error
  "The dimensions of axis-numbers~%"
  "  ~{~S~^~%in~%  ~}~%"
  "and~%"
  "  ~{~S~^~%in~%  ~}~%"
  "unmatched.~%")

(define-blas-unmatch-condition total-size-unmatch-error
  "The total-size of~%"
  "  ~S~%"
  "and~%"
  "  ~S~%"
  "unmatched.~%")


;;; Assert Macros
(defmacro assert-dimensions (blas-array dimensions)
  `(assert (equal (blas-array-dimensions ,blas-array) ,dimensions) (,blas-array)
           'dimensions-error :datum ,blas-array :expected-type ,dimensions))

(defmacro assert-rank (blas-array rank)
  `(assert (= (blas-array-rank ,blas-array) ,rank) (,blas-array)
           'rank-error :datum ,blas-array :expected-type ,rank))

(defmacro assert-dimension (blas-array axis-number dimension)
  `(assert (= (blas-array-dimension ,blas-array ,axis-number) ,dimension)
           (,blas-array)
           'dimension-error
           :datum ,blas-array
           :expected-type (list ,axis-number ,dimension)))

(defmacro assert-total-size (blas-array total-size)
  `(assert (= (blas-array-total-size ,blas-array) ,total-size) (,blas-array)
           'total-size-error :datum ,blas-array :expected-type ,total-size))

(defmacro assert-dimensions-match (blas-array1 blas-array2)
  `(assert (equal (blas-array-dimensions ,blas-array1)
                  (blas-array-dimensions ,blas-array2))
           (,blas-array1 ,blas-array2)
           'dimensions-unmatch-error :datum1 ,blas-array1 :datum2 ,blas-array2))

(defmacro assert-rank-match (blas-array1 blas-array2)
  `(assert (= (blas-array-rank ,blas-array1)
              (blas-array-rank ,blas-array2))
           (,blas-array1 ,blas-array2)
           'rank-unmatch-error :datum1 ,blas-array1 :datum2 ,blas-array2))

(defmacro assert-dimension-match (blas-array1 axis-number1 blas-array2 axis-number2)
  `(assert (= (blas-array-dimension ,blas-array1 ,axis-number1)
              (blas-array-dimension ,blas-array2 ,axis-number2))
           (,blas-array1 ,blas-array2)
           'dimension-unmatch-error
           :datum1 (list ,axis-number1 ,blas-array1)
           :datum2 (list ,axis-number2 ,blas-array2)))

(defmacro assert-total-size-match (blas-array1 blas-array2)
  `(assert (= (blas-array-total-size ,blas-array1)
              (blas-array-total-size ,blas-array2))
           (,blas-array1 ,blas-array2)
           'total-size-unmatch-error :datum1 ,blas-array1 :datum2 ,blas-array2))
