(defpackage cl-tensor.tensor
  (:use :common-lisp)
  (:nicknames :clt.tensor)
  (:export
    ;; core
    #:tensor
    #:*cublas-handle*
    #:with-handle

    ;;; BLAS level-1
    ;#:axpy
    ;#:asum
    ;#:copy
    ;#:dot
    ;#:nrm2
    ;#:scal
    ;#:amax
    ;#:amin

    ;;; BLAS level-2
    ;#:gemv
    ;#:ger

    ;;; BLAS level-3
    ;#:gemm

    ;;; BLAS-like extensions
    ;#:geam
    ))
(in-package :cl-tensor.tensor)


;; core
(defparameter *default-data-type* :float)
(defparameter *handle* nil)

(defstruct (tensor (:constructor %tensor)
                   (:copier nil)
                   (:predicate tensor?))
  (dimensions nil :read-only t)
  (handle     nil :read-only t)
  (data-type  nil :read-only t)
  (data       nil)
  (creator    nil :read-only t))

(defun tensor-rank (instance)
  (check-type instance tensor)
  (length (tensor-dimensions instance)))


(defun tensor (dimensions &key handle data-type)
  (check-type dimensions list)
  (mapc (lambda (dim) (check-type dim integer)) dimensions)
  (check-type data-type (member :float :double cu-complex cu-double-complex))
  (let ((data-type (or data-type *default-data-type*))
        (handle (or handle (clt.cublas:create-handle))))
    (%tensor :dimensions dimensions
             :handle     handle 
             :data-type  data-type
             :data       (clt.cublas:make-device-pointer
                           data-type
                           (reduce #'+ dimensions))
             :creator    nil)))

(defun destroy-tensor (tensor)
  (clt.cublas:destroy (tensor-handle tensor))
  (clt.cublas:cuda-free (tensor-data tensor)))


(defun call-with-tensor (dimensions function &key handle data-type)
  (let ((tensor (tensor dimensions :handle handle :data-type data-type)))
    (unwind-protect (funcall function tensor)
      (destroy-tensor tensor))))

(call-with-tensor
  '(3 4)
  (lambda (ten)
    ()
    )
  )


;;; constructor
(defun zeros-tensor (dimensions &key handle data-type)
  (let ((ten (tensor dimensions :handle))))
  )



#|
(defparameter *cublas-types* :float)
(defparameter *cublas-handle* nil)

(defmacro with-cublas ((&optional var handle) &body body)
  (if handle
      `(let ((,var ,handle))
         ,@body)
      `(with-handle ,(or var '*cublas-handle*)
                    ,@body)))
|#
