(in-package :cl-tensor.blas.cblas)


(define-foreign-library libopenblas
  (:unix "libopenblas.so")
  (t (:default "libopenblas")))
(use-foreign-library libopenblas)


;;; Define openBLAS level-1 functions
(defcfun* ("cblas_i?amax" ?amax) cblas-index
  (n blasint)
  (=>x :pointer)
  (incx blasint)
  (=>y :pointer)
  (incy blasint))

(defcfun* ("cblas_i?amin" ?amin) cblas-index
  (n blasint)
  (=>x :pointer)
  (incx blasint)
  (=>y :pointer)
  (incy blasint))

(defcfun* ("cblas_?axpy" ?axpy) :void
  (n blasint)
  (alpha ?type)
  (=>x :pointer)
  (incx blasint)
  (incy blasint))

(defcfun* ("cblas_?copy" ?copy) :void
  (n blasint)
  (alpha ?type)
  (=>x :pointer)
  (incx blasint)
  (=>y :pointer)
  (incy blasint))

(defcfun* ("cblas_?dot" ?dot :real-only? t) ?type
  (n blasint)
  (=>x :pointer)
  (incx blasint)
  (=>y :pointer)
  (incy blasint))

(defcfun* ("cblas_?gemm" ?gemm) :void
  (order cblas-order)
  (transa cblas-transpose)
  (transb cblas-transpose)
  (m blasint)
  (n blasint)
  (k blasint)
  (alpha ?type)
  (=>a :pointer)
  (lda blasint)
  (=>b :pointer)
  (ldb blasint)
  (beta ?type)
  (=>c :pointer)
  (ldc blasint))
