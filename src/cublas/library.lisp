(in-package :cl-tensor.cublas)


;;; Define and Use libcublas
(define-foreign-library libcublas
  (:unix "libcublas.so")
  (t (:default "libcublas")))

(use-foreign-library libcublas)


;;; Define macro to define cublas_v2.h function
(defmacro defun/cublas_v2 ((cname lisp-name) &body args)
  `(defcfun (,(concatenate 'string cname "_v2") ,lisp-name) cublas-status
     ,@args))


;;; Define cuBLAS helper functions
(defun/cublas_v2 ("cublasCreate" cublas-create)
  (*handle :pointer))

(defun/cublas_v2 ("cublasDestroy" cublas-destroy)
  (handle cublas-handle))

(defmacro with-cublas-handle (var &body body)
  `(with-foreign-object (,var 'cublas-handle)
     (cublas-create ,var)
     (unwind-protect (multiple-value-prog1 (progn ,@body))
       (cublas-destroy (mem-ref ,var 'cublas-handle)))))

(defun/cublas_v2 ("cublasGetVersion" cublas-get-version)
  (handle cublas-handle)
  (*version :pointer))

(defcfun ("cublasGetProperty" cublas-get-property) cublas-handle
  (type library-property-type)
  (*value :pointer))

(defun/cublas_v2 ("cublasSetStream" cublas-set-stream)
  (handle cublas-handle)
  (stream-id cuda-stream))

(defun/cublas_v2 ("cublasSetWorkspace" cublas-set-workspace)
  (handle cublas-handle)
  (*workspace :pointer)
  (workspace-size-in-bytes :size))

(defun/cublas_v2 ("cublasGetStream" cublas-get-stream)
  (handle cublas-handle)
  (*stream-id :pointer))

(defun/cublas_v2 ("cublasGetPointerMode" cublas-get-pointer-mode)
  (handle cublas-handle)
  (*mode :pointer))

(defun/cublas_v2 ("cublasSetPointerMode" cublas-set-pointer-mode)
  (handle cublas-handle)
  (*mode :pointer))

(defun/cublas_v2 ("cublasSetVector" cublas-set-vector)
  (n :int)
  (elem-size :int)
  (*x :pointer)
  (incx :int)
  (*y :pointer)
  (incy :int))

(defun/cublas_v2 ("cublasGetVector" cublas-get-vector)
  (n :int)
  (elem-size :int)
  (*x :pointer)
  (*y :pointer)
  (incy :int))

(defun/cublas_v2 ("cublasSetMatrix" cublas-set-matrix)
  (rows :int)
  (cols :int)
  (elem-size :int)
  (*a :pointer)
  (lda :int)
  (*b :pointer)
  (ldb :int))

(defun/cublas_v2 ("cublasGetMatrix" cublas-get-matrix)
  (rows :int)
  (cols :int)
  (elem-size :int)
  (*a :pointer)
  (lda :int)
  (*b :pointer)
  (ldb :int))


;;; Define cuBLAS level-1 functions
(defun/cublas_v2 ("cublasIsamax" cublas-isamax)
  (handle cublas-handle)
  (n :int)
  (x :pointer)
  (incx :int)
  (result :pointer))

#|
(with-cublas-handle *handle
  (with-foreign-objects ((*version :int)
                         (*type 'library-property-type_t)
                         (*value :int))
    (cublas-get-version (mem-ref *handle 'cublas-handle_t) *version)
    (format t "cublasGetVersion: ~A~%" (mem-ref *version :int))
    (cublas-get-property (mem-ref *type 'library-property-type_t) *value)
    (format t "cublasGetProperty: ~A~%" (mem-ref *value :int))))
|#
