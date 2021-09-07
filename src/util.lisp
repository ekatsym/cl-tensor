(defpackage cl-tensor.util
  (:nicknames :clt.util :clt.ut)
  (:use :common-lisp)
  (:export
    #:index
    #:mappend
    #:zip
    #:partial
    #:indices
    #:index->row-major-index
    #:index->col-major-index))
(in-package :cl-tensor.util)


(deftype index ()
  `(integer 0 ,array-dimension-limit))

(defun mappend (function list &rest more-lists)
  (reduce (lambda (xs acc) (append (apply function xs) acc))
          (apply #'mapcar #'list (cons list more-lists))
          :initial-value '()
          :from-end t))

(defun zip (&rest lists)
  (apply #'mapcar #'list lists))

(defun partial (fn &rest args)
  (lambda (&rest rest-args) (apply fn (append args rest-args))))

(defun indices (dimensions)
  (reduce (lambda (dim acc)
            (loop for i below dim
                  append (mapcar (lambda (idx) (cons i idx)) acc)))
          dimensions
          :initial-value '(())
          :from-end t))

(defun index->row-major-index (index dimensions)
  (reduce (lambda (acc idim)
            (destructuring-bind (i dim) idim
              (+ i (* dim acc))))
          (zip index dimensions)
          :initial-value 0))

(defun index->col-major-index (index dimensions)
  (reduce (lambda (idim acc)
            (destructuring-bind (i dim) idim
              (+ i (* dim acc))))
          (zip index dimensions)
          :initial-value 0
          :from-end t))
